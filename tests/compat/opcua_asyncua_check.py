#!/usr/bin/env python3
"""TOPCUAServer seen by another OPC UA stack: python-asyncua (not open62541).

The suite (tests/ut.opcuaserver.pas) talks to the server with open62541 itself;
a misreading shared by both sides would pass there. This script checks the
same things from asyncua, against tests/compat/opcua_compat_server:

  identity (ApplicationUri/Name), browsing, the native types, reads,
  subscriptions, writes (good, refused, wrong type, index range), and a
  SignAndEncrypt session with a user (the certificate URI included).

  python3 -m venv venv && venv/bin/pip install asyncua
  venv/bin/python tests/compat/opcua_asyncua_check.py [path of opcua_compat_server]

OPEN62541_LIB is passed on to the server. Exit code 0 when every check passes.
"""

import asyncio
import os
import shutil
import socket
import subprocess
import sys
import tempfile

from asyncua import Client, ua
from asyncua.crypto.security_policies import SecurityPolicyBasic256Sha256

HERE = os.path.dirname(os.path.abspath(__file__))
PORT = 24841
URL = f"opc.tcp://localhost:{PORT}"
NS_URI = "urn:pascalscada:tags"

failures = []
passed = 0


def check(cond, what):
    global passed
    if cond:
        passed += 1
        print(f"  ok   {what}")
    else:
        failures.append(what)
        print(f"  FAIL {what}")


async def status_of(coro):
    """The StatusCode name an operation ends with ('Good' when it does not raise)."""
    try:
        await coro
        return "Good"
    except ua.UaStatusCodeError as e:
        return type(e).__name__
    except Exception as e:  # noqa: BLE001 - reported as the check's result
        return f"{type(e).__name__}: {e}"


async def child(node, name):
    for c in await node.get_children():
        if (await c.read_browse_name()).Name == name:
            return c
    return None


async def names(node):
    return sorted([(await c.read_browse_name()).Name for c in await node.get_children()])


class Changes:
    def __init__(self):
        self.values = []

    def datachange_notification(self, node, val, data):
        self.values.append(val)


async def anonymous():
    print("anonymous, no security")
    async with Client(URL) as c:
        # identity
        eps = await c.get_endpoints()
        check(eps and eps[0].Server.ApplicationUri == "urn:pascalscada:compat",
              f"ApplicationUri of the endpoints ({eps[0].Server.ApplicationUri if eps else None})")
        check(eps and eps[0].Server.ApplicationName.Text == "PascalSCADA compat",
              f"ApplicationName ({eps[0].Server.ApplicationName.Text if eps else None})")
        ns = await c.get_namespace_index(NS_URI)
        check(ns > 0, f"namespace {NS_URI} is ns={ns}")

        # browsing
        root = await child(c.nodes.objects, "PascalSCADA")
        check(root is not None, "Objects/PascalSCADA")
        check(await names(root) == sorted(["Counter", "Setpoint", "SetpointBit0", "Linha1", "Message",
                                            "Trend", "Recipe"]),
              f"what the server folder holds: {await names(root)}")
        line = await child(root, "Linha1")
        check(await names(line) == ["Forno", "Ratio"], "Linha1 holds Forno and Ratio")
        oven = await child(line, "Forno")
        temp = await child(oven, "Temperatura")
        check(temp is not None and temp.nodeid == ua.NodeId("Linha1.Forno.Temperatura", ns),
              "Linha1/Forno/Temperatura with its string NodeId")
        recipe = await child(root, "Recipe")
        check(await recipe.read_node_class() == ua.NodeClass.Object, "a structure is an object")
        check(await names(recipe) == ["Code", "Gain", "Speed"], "with its items")
        desc = await (await child(root, "Counter")).read_description()
        check(desc.Text == "Counts every 100 ms", "the description of an item")

        node = lambda ident: c.get_node(ua.NodeId(ident, ns))  # noqa: E731

        # types and values
        expected = {
            "Counter": ua.VariantType.Int32, "Setpoint": ua.VariantType.UInt16,
            "SetpointBit0": ua.VariantType.Boolean, "Linha1.Ratio": ua.VariantType.Float,
            "Linha1.Forno.Temperatura": ua.VariantType.Int16, "Message": ua.VariantType.String,
            "Trend": ua.VariantType.Int16, "Recipe.Speed": ua.VariantType.UInt16,
            "Recipe.Gain": ua.VariantType.Float, "Recipe.Code": ua.VariantType.String,
        }
        for ident, vt in expected.items():
            dv = await node(ident).read_data_value()
            check(dv.Value.VariantType == vt and dv.StatusCode.is_good(),
                  f"{ident} is {vt.name} ({dv.Value.VariantType.name}, {dv.StatusCode.name})")
        check(await node("Linha1.Forno.Temperatura").read_value() == -12, "Int16 -12")
        check(await node("Message").read_value() == "hello", "the string")
        check(await node("Trend").read_value() == [1, 2, 3, 4, 5], "the array")
        check(await node("Recipe.Code").read_value() == "ab", "the string in the structure")
        check(abs(await node("Recipe.Gain").read_value() - 2.5) < 1e-6, "the float in the structure")
        dv = await node("Counter").read_data_value()
        check(dv.SourceTimestamp is not None, "a source timestamp")
        dt = await node("Trend").read_data_type()
        check(dt == ua.NodeId(ua.ObjectIds.Int16), "DataType attribute of the array")
        check(await node("Trend").read_value_rank() == 1, "ValueRank of the array")
        check(await node("Trend").read_array_dimensions() == [5], "ArrayDimensions of the array")
        # an index range read
        rv = ua.ReadValueId()
        rv.NodeId = ua.NodeId("Trend", ns)
        rv.AttributeId = ua.AttributeIds.Value
        rv.IndexRange = "1:3"
        params = ua.ReadParameters()
        params.NodesToRead = [rv]
        res = await c.uaclient.read(params)
        check(res[0].StatusCode.is_good() and res[0].Value.Value == [2, 3, 4], f"index range 1:3 ({res[0].Value.Value})")

        # subscription
        handler = Changes()
        sub = await c.create_subscription(50, handler)
        await sub.subscribe_data_change(node("Counter"), sampling_interval=20)
        await asyncio.sleep(1.5)
        vals = handler.values
        check(len(vals) >= 5 and vals == sorted(vals) and len(set(vals)) == len(vals),
              f"the counter is notified as it counts ({len(vals)} notifications)")
        await sub.delete()

        # writes
        sp = node("Setpoint")
        check(await status_of(sp.write_value(ua.DataValue(ua.Variant(300, ua.VariantType.UInt16)))) == "Good",
              "write UInt16 300")
        check(await sp.read_value() == 300, "read back 300")
        check(await node("SetpointBit0").read_value() is False, "bit 0 of 300 is False")
        check(await status_of(node("SetpointBit0").write_value(
            ua.DataValue(ua.Variant(True, ua.VariantType.Boolean)))) == "Good", "write the bit")
        check(await sp.read_value() == 301, "the bit went into its word (301)")
        st = await status_of(sp.write_value(ua.DataValue(ua.Variant(5, ua.VariantType.Int32))))
        check(st == "BadTypeMismatch", f"the wrong type is refused ({st})")
        st = await status_of(sp.write_value(ua.DataValue(ua.Variant(70000, ua.VariantType.UInt32))))
        check(st == "BadTypeMismatch", f"an UInt32 for an UInt16 is refused ({st})")
        st = await status_of(node("Counter").write_value(ua.DataValue(ua.Variant(1, ua.VariantType.Int32))))
        check(st in ("BadNotWritable", "BadUserAccessDenied"), f"a read only variable ({st})")
        st = await status_of(node("Linha1.Ratio").write_value(ua.DataValue(ua.Variant(1.0, ua.VariantType.Float))))
        check(st == "BadUserAccessDenied", f"OnCanWrite refuses the anonymous user ({st})")
        check(await status_of(node("Message").write_value(
            ua.DataValue(ua.Variant("olá", ua.VariantType.String)))) == "Good", "write a string (UTF-8)")
        check(await node("Message").read_value() == "olá", "read it back")
        check(await status_of(node("Recipe.Speed").write_value(
            ua.DataValue(ua.Variant(77, ua.VariantType.UInt16)))) == "Good", "write a structure item")
        check(await node("Recipe.Speed").read_value() == 77, "read it back")
        # a write with timestamps (some clients send them)
        dv = ua.DataValue(ua.Variant(310, ua.VariantType.UInt16))
        dv.SourceTimestamp = dv.ServerTimestamp = None
        check(await status_of(sp.write_value(dv)) == "Good", "a DataValue without timestamps")
        # index range write
        wv = ua.WriteValue()
        wv.NodeId = ua.NodeId("Trend", ns)
        wv.AttributeId = ua.AttributeIds.Value
        wv.IndexRange = "1:2"
        wv.Value = ua.DataValue(ua.Variant([20, 30], ua.VariantType.Int16))
        params = ua.WriteParameters()
        params.NodesToWrite = [wv]
        res = await c.uaclient.write(params)
        check(res[0].is_good(), f"index range write 1:2 ({res[0].name})")
        check(await node("Trend").read_value() == [1, 20, 30, 4, 5], "read back the array")


async def secure(tmp):
    print("user op over Basic256Sha256 SignAndEncrypt")
    from asyncua.crypto.cert_gen import generate_private_key, generate_self_signed_app_certificate, dump_private_key_as_pem
    from cryptography import x509
    from cryptography.hazmat.primitives import serialization
    from cryptography.x509.oid import ExtendedKeyUsageOID

    app_uri = "urn:pascalscada:compat:client"
    pk = generate_private_key()
    cert = generate_self_signed_app_certificate(
        pk, app_uri, {"commonName": "asyncua compat"},
        [x509.UniformResourceIdentifier(app_uri), x509.DNSName("localhost"), x509.DNSName(socket.gethostname())],
        extended=[ExtendedKeyUsageOID.CLIENT_AUTH])
    cert_file = os.path.join(tmp, "client_cert.der")
    key_file = os.path.join(tmp, "client_key.pem")
    with open(cert_file, "wb") as f:
        f.write(cert.public_bytes(serialization.Encoding.DER))
    with open(key_file, "wb") as f:
        f.write(dump_private_key_as_pem(pk))

    async def connect(user, password):
        c = Client(URL)
        c.application_uri = app_uri
        c.set_user(user)
        c.set_password(password)
        await c.set_security(SecurityPolicyBasic256Sha256, cert_file, key_file,
                             mode=ua.MessageSecurityMode.SignAndEncrypt)
        await c.connect()
        return c

    try:
        c = await connect("op", "123")
    except Exception as e:  # noqa: BLE001
        check(False, f"op/123 logs in ({type(e).__name__}: {e})")
        return
    check(True, "op/123 logs in")
    try:
        # the certificate of the server is for its ApplicationUri
        server_cert = c.security_policy.peer_certificate
        san = x509.load_der_x509_certificate(server_cert).extensions.get_extension_for_class(
            x509.SubjectAlternativeName).value
        uris = san.get_values_for_type(x509.UniformResourceIdentifier)
        check("urn:pascalscada:compat" in uris, f"the server certificate is for urn:pascalscada:compat ({uris})")
        ns = await c.get_namespace_index(NS_URI)
        ratio = c.get_node(ua.NodeId("Linha1.Ratio", ns))
        st = await status_of(ratio.write_value(ua.DataValue(ua.Variant(1.5, ua.VariantType.Float))))
        check(st == "Good", f"a logged user writes Linha1.Ratio ({st})")
        check(abs(await ratio.read_value() - 1.5) < 1e-6, "read back 1.5")
    finally:
        await c.disconnect()

    try:
        c = await connect("op", "wrong")
        await c.disconnect()
        check(False, "a wrong password is refused")
    except Exception as e:  # noqa: BLE001
        check(True, f"a wrong password is refused ({type(e).__name__})")


async def main():
    exe = sys.argv[1] if len(sys.argv) > 1 else os.path.join(HERE, "bin", "opcua_compat_server")
    if os.name == "nt" and not exe.lower().endswith(".exe"):
        exe += ".exe"
    tmp = tempfile.mkdtemp(prefix="pscada-asyncua-")
    srv = subprocess.Popen([exe, str(PORT), os.path.join(tmp, "pki"), "90"],
                           stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    try:
        for line in srv.stdout:
            if line.startswith("READY"):
                break
            if line.startswith("NOT RUNNING"):
                print(line.strip())
                return 1
        else:
            print("the server ended before serving")
            return 1
        # keep reading its output, or it blocks on a full pipe
        loop = asyncio.get_running_loop()
        loop.run_in_executor(None, lambda: [None for _ in srv.stdout])
        await anonymous()
        await secure(tmp)
    finally:
        srv.terminate()
        try:
            srv.wait(10)
        except subprocess.TimeoutExpired:
            srv.kill()
        shutil.rmtree(tmp, ignore_errors=True)
    print(f"\n{passed} passed, {len(failures)} failed")
    for f in failures:
        print(f"  - {f}")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(asyncio.run(main()))
