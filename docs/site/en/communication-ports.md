##### Introduction

Communication ports are the way used by the protocol drivers to send and receive data. One communication port can be used by one or more protocol drivers, simultaneously.

Communication ports aren’t implemented together with protocol drivers to allow more flexibility to the system. An real application that can be done using this architecture, is the association of a TTCP_UDPPort with a COMServer (devices that acts as a gateway of RS-232/485/422 to TCP/IP), to by example, communicate with a Modbus RTU device that is far, over the Internet. Another advantage of this design is to put two or more different protocols using the same communication port simultaneously, both on the same application. This is possible because all PascalSCADA ports comes with mechanism that only allows one protocol to use communication port. That is, an application can communicate with multiple devices, these devices with different communication protocols and all connected to the same RS-485 network. It is possible, though not recommended.

All port classes that exists on PascalSCADA descend from TCommPort class, that implements the methods to open and close the port, read and write data, and clean the buffers when a communication error occurs. But if a new port is being written, these methods should be specialized to handle the new port being implemented.

##### ![Seleção_090](http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_090.png)TSerialPort

The TSerialPort class, descendant of TCommPort class, has been created to allow the protocol drivers to exchange data over serial ports, regardless of the physical layer used by serial port.

On Windows, this class need of a serial port with name between COM1 and COM255.

Running over Linux, any device where the name starts with tty&lt;number&gt;, ttyUSB&lt;number&gt; or ttyACM&lt;number&gt; is valid.

On FreeBSD, any device where the name starts with cuad&lt;number&gt; is accepted by the class.

**Both Linux and FreeBSD (or another Unix) should be added the user that will run the application on the group owner of the serial port and the group must have permission of read/write on the desired serial port. The name of this group can vary depending of the operating system.**

Independent of operating system, the TSerialPort class has the following properties that configure the operation of the serial port:

  * **COMPort:** defines the name of serial port to be used. If the application has been configured with one serial port and it is disconnected of the system (by example, a USB &lt;-&gt; serial converter) and the application is started, a exception will be raised. If this exception occurs on the application load (initialization), your application will be not loaded, even if Active=false.
  * **Baudrate** : communication speed in bits per second that will be used on the configured serial port while it’s in use. Baud rates between 110 bps and 115200 bps are accepted.
  * **DataBits** : Defines the word length in bits that will be used when communicating with the serial port. Only the size of 7 or 8 bits are accepted, the default is 8 bits. Some protocols requires the value of this property set to 7 bits, like Modbus ASCII. Others requires the word size of 8 bits, like Mobus RTU.
  * **StopBits** : Configure how many bits will be used to mark start and the end of each word transmitted or received in the serial communication. Valid values are 1 or 2 stop bits. The default is 1 stop bit.
  * **Paridade:** configures if each word received word will have a parity error check, and if error check exists, this defines if it will be even or odd.
  * **Timeout** : defines the maximum time that the serial port will wait for reading a certain amount of data. This time is set in milliseconds. In cases of timeout overflow, a clean up of the read and write buffers is performed.
  * **AcceptAnyPortName** : if this property is enabled on a Unix system (Linux, FreeBSD, etc), makes the TSerialPort accept any device name on the COMPort property. Very useful when some some drivers create devices with names that don’t matches the patterns described above.
  * **WriteReadDelay** : Delay in milliseconds between write and read commands.
  * **Active:** This property controls when the port will be opened or closed. The serial port should not be open by another process when this property goes to true, otherwise the value of this property come back to false. In the development time, Active = true only checks if everything is OK to open the serial port, but don’t open it.

##### ![Seleção_091](http://www.pascalscada.com/wp-content/uploads/2016/08/Seleção_091.png)TTCP_UDPPort

The class TTCP_UDPPort, descendant of TCommport, has been created to allow the protocol drivers exchange data over TCP (tested) or UPD (not tested) over IPv4, independent of physical and link layer being used by the port.

This class has no operating system dependent properties, and the following properties configures its operation:

  * **Host** : The IPv4 address of the host to connect. Host names are not accepted, only IPv4 address.
  * **Port** : Port number on the destination host to connect. Only port numbers between 1 and 65535 are accepted.
  * **PortType** : Sets the socket type that will created when connected with the host. It can be a TCP or UDP.
  * **ExclusiveDevice** : If true, don’t opens the socket when in the development time. Useful when the destination host has a few numbers of available connections.
  * **EnableAutoReconnect** : If true and if the socket is disconnected (problem on the physical layer or on the host) enables the timer that tries to reconnect the socket. In this case the invalid socket is closed while Active property remains true.
  * **ReconnectRetryInterval** : This property defines the interval in milliseconds to attempt to reconnect lost connections. This property is only valid if both EnableAutoReconnect and Active are true.
  * **Timeout** : Sets the maximum time to wait to complete a reading or writing of a certain amount of data. This time is set in milliseconds. If any operation exceeds this time limit, the buffers are cleaned up, the socket is verified and if some trouble is detected, the current socket will be closed and if EnableAutoReconnect is true, a attempt to connect a new socket will be made.
  * **Active** : This property controls the opening and closing of the socket. At development time, Active = true will open the socket and communicate with your device unless ExclusiveDevice is set to true.

##### Examples

Bellow is listed a example of how exchange data using the PascalSCADA communication ports directly on the source code, in other words, without use a protocol driver. As all the communication ports are descendants of TCommPort class, the example below also applies if you want to implement something using TTCP_UDPPort.

  * Genius game, ranking interface: <https://sourceforge.net/p/pascalscada/code/HEAD/tree/trunk/examples/laz_serialport_genius/>
