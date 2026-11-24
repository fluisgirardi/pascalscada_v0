program mqtt_demo;

{$mode Delphi}{$H+}

uses
  Classes, SysUtils, PLCTagNumber, MQTTProtocol, MQTTClient, MQTTDriver;

var
  Client:TMQTTClient;
  Driver:TMQTTDriver;
  Tag:TPLCTagNumber;
  i:LongInt;
begin
  writeln('PascalSCADA MQTT 3.1.1 example');

  // --- 1) Low level client: publish/subscribe on a topic ---
  Client := TMQTTClient.Create;
  try
    Client.Host := '127.0.0.1';
    Client.Port := 1883;
    Client.ClientID := 'pascalscada_demo';
    if Client.Connect then begin
      writeln('MQTT client connected');
      if Client.Publish('demo/test', '12.5', mqQoS1, false, 3000) then
        writeln('Published 12.5 on demo/test')
      else
        writeln('Publish failed: ', Client.LastError);
      Client.Disconnect;
    end else
      writeln('Could not connect: ', Client.LastError);
  finally
    Client.Free;
  end;

  // --- 2) PascalSCADA driver: a tag bound to an MQTT topic ---
  Driver := TMQTTDriver.Create(nil);
  Tag := TPLCTagNumber.Create(nil);
  try
    Driver.Host := '127.0.0.1';
    Driver.Port := 1883;
    Driver.QoS := mqQoS1;

    Tag.ProtocolDriver := Driver;
    Tag.LongAddress := 'demo/temperature';
    Tag.UpdateTime := 1000;

    writeln('Driver configured: tag topic=', Tag.LongAddress);
    writeln('Start the scan engine (TProtocolDriver.AddTag / scanner) in your app.');
  finally
    Tag.Free;
    Driver.Free;
  end;

  writeln('Done.');
end.
