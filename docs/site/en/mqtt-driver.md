# MQTT 3.1.1 protocol driver (TMQTTDriver)

![](img/TMQTTDriver.png) `TMQTTDriver` connects PascalSCADA tags to an MQTT
broker. It is available on the **PascalSCADA Protocols** component palette.

## Protocol support

* MQTT **3.1.1** (protocol level 4), over plain TCP.
* QoS **0** and **1** (QoS 2 is not implemented).
* CONNECT / CONNACK, SUBSCRIBE / SUBACK, PUBLISH / PUBACK, PINGREQ / PINGRESP,
  DISCONNECT, automatic reconnect, automatic re-subscription.
* Optional user name / password in CONNECT (no TLS in this version).

## Tag addressing

Each tag's **LongAddress** is the MQTT topic, for example `plant/line1/temp`.

The driver owns the MQTT connection; it does **not** need a `TTCP_UDPPort`
component. Follow the same rule as `TOPCUADriver`: leave `CommunicationPort`
unassigned.

### Value conversion

* Numeric payloads (`23.5`, `true`, `false`, `on`, `off`, `0x1A`) are converted
  to numbers and delivered to `TPLCTagNumber`, `TPLCBlock`, etc.
* Non numeric payloads are delivered as UTF-8 byte codes, for `TPLCString`.
* Writing a numeric tag publishes a decimal number; writing a `TPLCString`
  tag publishes its UTF-8 text.
* For blocks (`TPLCBlock`), values are published comma separated.

## Properties

| Property | Default | Description |
|---|---|---|
| `Host` | empty | Broker host name or IP. |
| `Port` | 1883 | Broker TCP port. |
| `ClientID` | empty | MQTT client id. Empty generates one automatically. |
| `UserName` / `Password` | empty | Optional MQTT credentials. |
| `KeepAlive` | 60 | Keep alive interval in seconds. |
| `CleanSession` | true | CONNECT clean-session flag. |
| `QoS` | mqQoS1 | QoS for subscriptions and publications. |
| `Retain` | false | RETAIN flag of published values. |
| `ConnectTimeout` | 3000 | TCP connect and CONNACK timeout, ms. |
| `WriteTimeout` | 3000 | PUBACK wait timeout, ms. |
| `ReconnectInterval` | 3000 | Delay between connection attempts, ms. |
| `ReadOnly` | false | Inherited from the base driver. |

## Runtime API

```pascal
if Driver.Connected then writeln('online');
writeln(Driver.LastError);

// Publish outside the tag scan (no tag required):
Driver.PublishTopic('plant/line1/command', 'START');
```

## Units

| Unit | Purpose |
|---|---|
| `src/scada/mqttprotocol.pas` | Pure MQTT 3.1.1 packet codec, including a stream decoder that handles split/coalesced TCP frames. |
| `src/scada/mqttclient.pas` | TCP client (FPC `ssockets`), connection state, PUBACK/PING handling. |
| `src/scada/mqttdriver.pas` | `TMQTTDriver`, tag mapping and PascalSCADA driver integration. |

## Example

`examples/laz_mqtt_console/mqtt_demo.pas` shows both the low level client and
the driver. For a quick try, an MQTT broker on `127.0.0.1:1883` (e.g. Mosquitto)
is enough.

## Status / limitations

* No TLS / MQTTS yet.
* No QoS 2.
* No Last Will and Testament.
* No automatic topic discovery (`ListTags`).
