unit mav1defs;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, math, DateUtils;

const
  dzfl='0.0';                {Formatierung für FormatFloat}
  ctfl='0.00';
  mlfl='0.000';
  coordfl8='0.00000000';

  NumPayloadBytes=255;       {reserved lenght for data}

  LengthFixPartBC=6;
  LengthFixPartFD=10;
  LengthFixPartFE=8;
  MagicBC=$BC;
  MagicFD=$FD;
  MagicFE=$FE;
  MagicFE_AsText='0xFE';
  MagicFD_AsText='0xFD';

  rsUnknown_='Unknown_';
  rsUndef_='Undef_';


type
  TMAVmessage = record
    time: TDateTime;
    msglength: integer;
    msgbytes: array[0..NumPayLoadBytes+LengthFixPartFE+2] of byte;
    sysid: byte;
    targetid: byte;
    msgid: byte;
    msgid32: uint32;
    valid: boolean;
  end;

{Public functions and procedures}
function CRC16X25(const msg: TMAVmessage; LengthFixPart: byte): uint16;
function CheckCRC16X25(const msg: TMAVmessage; LengthFixPart: byte): boolean;
procedure CRC_accumulate(const b: byte; var crcAccum: uint16);
function CRC16MAV(const msg: TMAVmessage; LengthFixPart: byte; startpos: byte=1): uint16;
function CheckCRC16MAV(const msg: TMAVmessage; LengthFixPart: byte): boolean;

function MavGetUInt64(msg: TMAVmessage; pos: integer): uint64; {Position/Anzahl Bytes}
function MavGetFloatFromBuf(msg: TMAVmessage; pos: integer): single; {Position, Länge immer 4}
function MavGetUInt16(msg: TMAVmessage; pos: integer): uint16;
function MavGetInt16(msg: TMAVmessage; pos: integer): int16;
function MavGetUInt32(msg: TMAVmessage; pos: integer): uint32;
function MavGetInt32(msg: TMAVmessage; pos: integer): int32;

function ClearMAVmessage: TMAVmessage;
function MsgIDToStr_FE(id: byte): string;
function SysIDToStr(id: byte): string;
function TargetIDToStr(id: byte): string;
function SensorTypeToStr(id: byte): string;  {Message BC}
function MAV_PARAM_TYPEtoStr(const id: byte): string; {Specifies the datatype of a MAVLink parameter}
function MAV_RESULTtoStr(const id: byte): string;
function YGC_TypeToStr(y: byte): string;
function YGC_CommandToStr(y: byte): string;
function SeverityToStr(severity: byte): string;

function IntToYaw(v: integer): string;
function IntToAngle(v: integer): string;
function IntToPan(v: integer): string;
function IntToTilt(v: integer): string;
function IntToPanMode(v: integer): string;
function IntToTiltMode(v: integer): string;
function IntToLatLon(v: int32): string;
function FloatToAlt(v: single): string;
function IntToAlt(v: int32): string;
function IntToAccuracy(v: integer): string;
function IntToRCAngle(v: int16): string;
function IntToSpeed(v: integer): string;
function GPSswitchToStr(g: byte): string;
function GimbalAngleToStr(a: int16): string;
function MavGetGPSdata(data: TMAVmessage; pos: integer;
                    var lat, lon, alt: single): boolean;  {Get lat, lon and alt from GPS data}
function VoltToStr(v: byte): string;
function MavGetChValue(data: TMAVmessage; chnr: byte; pos: integer=27): uint16; {Channel no from 1..12 or 1..24}
function GPSaccHToStr(const g: byte): string;
procedure MavGetInt123(data: TMAVmessage; pos: integer; var v1, v2, v3: int16);
procedure MavGetuInt123(data: TMAVmessage; pos: integer; var v1, v2, v3: uint16);
function GetSerialNumber(const msg: TMAVmessage; pos: byte): string;
function GetSystemTime(const data: TMAVmessage; pos: integer; var time: TDateTime): string;
function MsgIDtoStr(id: uint32): string;

implementation

{Tabelle CCITT X25 CRC aus ST16 MavLinkPackage.java
 b ... Array of Byte
 ln... Länge Payload (Byte 1) der Message, Byte 0=$BC wird nicht genutzt
       Schleife über Rest der Message 0...Länge Payload+Länge Fixpart-3}

function CRC16X25(const msg: TMAVmessage; LengthFixPart: byte): uint16;
const
  Crc16Tab: array[0..255] of Word = (
    $0000, $1189, $2312, $329B, $4624, $57AD, $6536, $74BF,
    $8C48, $9DC1, $AF5A, $BED3, $CA6C, $DBE5, $E97E, $F8F7,
    $1081, $0108, $3393, $221A, $56A5, $472C, $75B7, $643E,
    $9CC9, $8D40, $BFDB, $AE52, $DAED, $CB64, $F9FF, $E876,
    $2102, $308B, $0210, $1399, $6726, $76AF, $4434, $55BD,
    $AD4A, $BCC3, $8E58, $9FD1, $EB6E, $FAE7, $C87C, $D9F5,
    $3183, $200A, $1291, $0318, $77A7, $662E, $54B5, $453C,
    $BDCB, $AC42, $9ED9, $8F50, $FBEF, $EA66, $D8FD, $C974,
    $4204, $538D, $6116, $709F, $0420, $15A9, $2732, $36BB,
    $CE4C, $DFC5, $ED5E, $FCD7, $8868, $99E1, $AB7A, $BAF3,
    $5285, $430C, $7197, $601E, $14A1, $0528, $37B3, $263A,
    $DECD, $CF44, $FDDF, $EC56, $98E9, $8960, $BBFB, $AA72,
    $6306, $728F, $4014, $519D, $2522, $34AB, $0630, $17B9,
    $EF4E, $FEC7, $CC5C, $DDD5, $A96A, $B8E3, $8A78, $9BF1,
    $7387, $620E, $5095, $411C, $35A3, $242A, $16B1, $0738,
    $FFCF, $EE46, $DCDD, $CD54, $B9EB, $A862, $9AF9, $8B70,
    $8408, $9581, $A71A, $B693, $C22C, $D3A5, $E13E, $F0B7,
    $0840, $19C9, $2B52, $3ADB, $4E64, $5FED, $6D76, $7CFF,
    $9489, $8500, $B79B, $A612, $D2AD, $C324, $F1BF, $E036,
    $18C1, $0948, $3BD3, $2A5A, $5EE5, $4F6C, $7DF7, $6C7E,
    $A50A, $B483, $8618, $9791, $E32E, $F2A7, $C03C, $D1B5,
    $2942, $38CB, $0A50, $1BD9, $6F66, $7EEF, $4C74, $5DFD,
    $B58B, $A402, $9699, $8710, $F3AF, $E226, $D0BD, $C134,
    $39C3, $284A, $1AD1, $0B58, $7FE7, $6E6E, $5CF5, $4D7C,
    $C60C, $D785, $E51E, $F497, $8028, $91A1, $A33A, $B2B3,
    $4A44, $5BCD, $6956, $78DF, $0C60, $1DE9, $2F72, $3EFB,
    $D68D, $C704, $F59F, $E416, $90A9, $8120, $B3BB, $A232,
    $5AC5, $4B4C, $79D7, $685E, $1CE1, $0D68, $3FF3, $2E7A,
    $E70E, $F687, $C41C, $D595, $A12A, $B0A3, $8238, $93B1,
    $6B46, $7ACF, $4854, $59DD, $2D62, $3CEB, $0E70, $1FF9,
    $F78F, $E606, $D49D, $C514, $B1AB, $A022, $92B9, $8330,
    $7BC7, $6A4E, $58D5, $495C, $3DE3, $2C6A, $1EF1, $0F78);

var i: integer;

begin
  result:=$FFFF;                             {CRC Initializing}
  for i:=2 to LengthFixPart+msg.msglength-1 do begin
    result:=((result shr 8) and $00FF) xor CRC16Tab[(msg.msgbytes[i] xor result) and $00FF];
  end;
end;

function CheckCRC16X25(const msg: TMAVmessage; LengthFixPart: byte): boolean;
begin
  result:=(CRC16X25(msg, LengthFixPart+2)=0);
end;

procedure CRC_accumulate(const b: byte; var crcAccum: uint16);
var
  tmp: uint8;

begin
  tmp:=b xor (crcAccum and $00FF);
  tmp:=tmp xor (tmp shl 4);
  crcAccum:=(crcAccum shr 8) xor (tmp shl 8) xor (tmp shl 3) xor (tmp shr 4);
end;

{from checksum.h of MavlinkLib-master}
function CRC16MAV(const msg: TMAVmessage; LengthFixPart: byte; startpos: byte=1): uint16;
var
  i: integer;

begin
  result:=$FFFF;
  for i:=startpos to LengthFixPart+msg.msglength-1 do begin
    CRC_accumulate(msg.msgbytes[i], result);
  end;
  CRC_accumulate(0, result);
end;

function CheckCRC16MAV(const msg: TMAVmessage; LengthFixPart: byte): boolean;
begin
  result:=(CRC16MAV(msg, LengthFixPart)=MavGetUInt16(msg, LengthFixPart+msg.msglength));
end;

function MavGetUInt64(msg: TMAVmessage; pos: integer): uint64; {Position/Anzahl Bytes}
var
  i: integer;

begin
  result:=0;
  for i:=0 to 7 do begin
    result:=result+msg.msgbytes[pos+i]*(256**i);
  end;
end;

function MavGetFloatFromBuf(msg: TMAVmessage; pos: integer): single; {Position, Länge immer 4}
var i: integer;
    wfl: packed array[0..3] of Byte;
    wx: Single absolute wfl;

begin
  result:=0;
  for i:=0 to 3 do                               {Endianess prüfen (to/downto)}
    wfl[i]:=msg.msgbytes[i+pos];                 {4 byte aus Buffer ausschneiden}
  result:=wx;                                    {Typecast mittels absolute}
end;

function MavGetInt16(msg: TMAVmessage; pos: integer): int16;
begin
  result:=msg.msgbytes[pos]+msg.msgbytes[pos+1]*256;
end;

function MavGetUInt16(msg: TMAVmessage; pos: integer): uint16;
begin
  result:=msg.msgbytes[pos]+msg.msgbytes[pos+1]*256;
end;

function MavGetUInt32(msg: TMAVmessage; pos: integer): uint32;
var
  i: integer;

begin
  result:=0;
  for i:=0 to 3 do begin
    result:=result+msg.msgbytes[i+pos]*(256**i);
  end;
end;

function MavGetInt32(msg: TMAVmessage; pos: integer): int32;
var
  i: integer;

begin
  result:=0;
  for i:=0 to 3 do begin
    result:=result+msg.msgbytes[i+pos]*(256**i);
  end;
end;

function ClearMAVmessage: TMAVmessage;
var
  i: integer;

begin
  result.msglength:=0;
  result.sysid:=0;
  result.targetid:=0;
  result.valid:=false;
  for i:=0 to LengthFixPartFE do
    result.msgbytes[i]:=0;                    {Fix part empty}
end;

function MsgIDToStr_FE(id: byte): string;
begin
  result:=rsUnknown_+' '+IntToStr(id)+' (0x'+HexStr(id, 2)+')';
  case id of
    0: result:='HEARTBEAT';                   {1Hz}
    1: result:='Attitude';                    {Frequently}
    2: result:='Telemetry_5GHz';
    3: result:='Gimbal position';             {Sys_Id Camera?; 1Hz }
    8: result:='C-GPS_5GHz';
    20: result:='PARAM_REQUEST_READ';
    76: result:='Follow_on_param_request';    {Command long?? Passt nicht}
    255: result:='SensorData';
  end;
end;

function SysIDToStr(id: byte): string;        {Sent from}
begin
  result:=rsUnknown_+' '+IntToStr(id)+' (0x'+HexStr(id, 2)+')';
  case id of
    1: result:='Autopilot';                   {Flight controller (Autopilot)}
    2: result:='Gimbal';
    3: result:='Gimbal';
    4: result:='Remote';
    6: result:='SysID6';
    10: result:='YQGC';
    88: result:='Undef';
    200: result:='Tool/GUI';
  end;
end;

function TargetIDToStr(id: byte): string;     {Send to}
begin
  result:=rsUnknown_+' '+IntToStr(id)+' (0x'+HexStr(id, 2)+')';
  case id of
    0: result:='All (broadcast)';
    1: result:='Autopilot';                   {Flight controller (Autopilot)}
    2: result:='Gimbal';
    3: result:='Camera';
    5: result:='WiFi? (5)';
    10: result:='YQGC';
    88: result:='Undef';
    100: result:='Camera #1';
    154: result:='Gimbal #1';
    200: result:='Tool/GUI';
    99: result:='Remote';                    {Platzhalter für virtuelle Sender ID}
  end;
end;

{https://github.com/mavlink/mavlink/tree/master/message_definitions/v1.0
 https://github.com/mavlink/mavlink/blob/master/message_definitions/v1.0/common.xml
 }
function SensorTypeToStr(id: byte): string;  {Message BC}
begin
  result:=rsUnknown_+IntToStr(id);
  case id of
    0:   result:='Heartbeat';
    1:   result:='SYS_STATUS';
    2:   result:='SYSTEM_TIME';
    21:  result:='PARAM_REQUEST_LIST';       {Request all parameters of this component.
                                              After this request, all parameters are emitted.
                                              The parameter microservice is documented at
                                              https://mavlink.io/en/services/parameter.html}
    22:  result:='PARAM_VALUE';
    23:  result:='PARAM_SET';
    24:  result:='GPS_RAW_INT';
    25:  result:='GPS_STATUS';
    27:  result:='RAW_IMU';
    29:  result:='SCALED_PRESSURE';
    30:  result:='ATTITUDE';
    32:  result:='LOCAL_POSITION_NED';
    33:  result:='GLOBAL_POSITION';
    35:  result:='RC_CHANNELS_RAW';
    36:  result:='SERVO_OUTPUT_RAW';
    42:  result:='MISSION_CURRENT';
    51:  result:='MISSION_REQUEST_INT';
    52:  result:='System_type';                    {Text: CGO3_Plus / TyphoonH}
    56:  result:='SERIAL_NUMBER';
    62:  result:='NAV_CONTROLLER_OUTPUT';
    65:  result:='RC_CHANNELS';
    74:  result:='VRF_HUD';
    76:  result:='COMMAND_LONG';
    77:  result:='COMMAND_ACK';
    150: result:='SENSOR_OFFSETS';
    163: result:='AHRS';                           {Attitude and Heading Reference System}
    165: result:='HW_STATUS';
    172: result:='DATA96';
    173: result:='RANGEFINDER';
    178: result:='AHRS2';
    193: result:='EKF_STATUS_REPORT';              {Extended Kalman Filter}
    253: result:='STATUS_TEXT';
  end;
end;

function MAV_PARAM_TYPEtostr(const id: byte): string;        {Specifies the datatype of a MAVLink parameter}
begin
  result:='PARAM_TYPE '+intToStr(id);
  case id of
    1: result:='UINT8';
    2: result:='INT8';
    3: result:='UINT16';
    4: result:='INT16';
    5: result:='UINT32';
    6: result:='INT32';
    7: result:='UINT64';
    8: result:='INT64';
    9: result:='REAL32';
    10: result:='REAL64';
  end;
end;

function MAV_RESULTtoStr(const id: byte): string;
begin
  result:='RESULT '+intToStr(id);
  case id of
    0: result:='ACCEPTED';
    1: result:='TEMPORA';
    2: result:='DENIED';
    3: result:='UNSUPPORTED';
    4: result:='FAILED';
    5: result:='IN_PROGRESS';
    6: result:='CANCELLED';
    7: result:='COMMAND_LONG_ONLY';
    8: result:='COMMAND_INT_ONLY';
    9: result:='COMMAND_UNSUPPORTED_MAV_FRAME';
  end;
end;

function YGC_TypeToStr(y: byte): string;
begin
  result:=rsUndef_+IntToStr(y);
  case y of
    1: result:='GYRO_POWER';
    2: result:='EULER_ANGLE';
    3: result:='ACC';
    5: result:='TEMP_DIFF';
    6: result:='MSTATUS';
    18: result:='Undef 0x12 (18) 1Hz';
    254: result:='FW_Info';
  end;
end;

function YGC_CommandToStr(y: byte): string;
begin
  result:='YGC_Request ('+intToStr(y)+')';
  case y of
    20: result:='Front_cali?';
    24: result:='Read_SWversion';
    36: result:='Heartbeat';                    {Data request; starts sending data messages from Gimbal}
  end;
end;

function SeverityToStr(severity: byte): string;
begin
  result:=IntToStr(severity);
  case severity of
    0: result:='EMERGENCY';  {System is unusable. This is a "panic" condition}
    1: result:='ALERT    ';  {Action should be taken immediately. Indicates error
                             in non-critical systems}
    2: result:='CRITICAL ';  {Action must be taken immediately. Indicates failure
                             in a primary system}
    3: result:='ERROR    ';  {Indicates an error in secondary/redundant systems}
    4: result:='WARNING  ';  {Indicates about a possible future error if this
                             is not resolved within a given timeframe. Example
                             would be a low battery warning}
    5: result:='NOTICE   ';  {An unusual event has occurred, though not an error
                             condition. This should be investigated for the
                             root cause.}
    6: result:='INFO     ';  {Normal operational messages. Useful for logging.
                             No action is required for these messages.}
    7: result:='DEBUG    ';  {Useful non-operational messages that can assist in
                             debugging. These should not occur during normal
                             operation}
  end;
end;

function IntToYaw(v: integer): string;
begin
  result:=FormatFloat(dzfl, 180/16000*v-180);
end;

function IntToAngle(v: integer): string;
begin
  result:=FormatFloat(dzfl, -90/7650*v);
end;

function IntToPan(v: integer): string;
begin
//  result:=FormatFloat(dzfl, 100/2048*(v-2048));
  result:=intToStr(v);
end;

function IntToTilt(v: integer): string;
begin
  result:=FormatFloat(dzfl, -(v-683)*90/2729);
end;

function IntToTiltMode(v: integer): string;
begin
  result:=rsUndef_+IntToStr(v);
  case v of
    2048: result:='Neutral';                    {sollte eigentlich nicht auftreten}
    2184: result:='Angle';                      {+10%}
    3412: result:='Velocity';                   {+100%}
  end;
end;

function IntToPanMode(v: integer): string;
begin
  result:=rsUndef_+IntToStr(v);
  case v of
    683: result:='Follow';                      {-100%}
    1433: result:='Team';                       {-45%}
    1502: result:='Follow controllable';        {-40%}
    2048: result:='Neutral';                    {sollte eigentlich nicht auftreten}
    3412: result:='Global';                     {+100%}
  end;
end;

function IntToProzent(v: integer): string;
begin
  result:='Slide '+IntToStr(v)+' ('+IntToHex(v and 255, 2)+' '+            {nwt}
                                    IntToHex((v shr 8) and 255, 2)+')';    {hwt}
  case v of
    0: result:=   '-150%';
    683: result:= '-100%';
    1433: result:='-45%';
    1502: result:='-40%';
    2048: result:='0%';
    2184: result:='+10%';
    3412: result:='+100%';
    4095: result:='+150%';
  end;
end;

function IntToLatLon(v: integer): string;
begin
  result:=FormatFloat(coordfl8, v*0.0000001);
end;

function FloatToAlt(v: single): string;
begin
  result:=FormatFloat(ctfl, v);
end;

function IntToAlt(v: integer): string;
begin
  result:=FormatFloat(dzfl, v*0.01);
end;

function IntToAccuracy(v: integer): string;
begin
  result:=FormatFloat(dzfl, v*0.1);
end;

function IntToRCAngle(v: int16): string;
begin
  result:=FormatFloat(dzfl, v*0.01);
end;

function IntToSpeed(v: integer): string;
begin
  result:=FormatFloat(dzfl, v*0.1);
end;

function GPSswitchToStr(g: byte): string;
begin
  result:='0x'+IntToHex(g, 2);         {currently always 0x0B}
end;

function GimbalAngleToStr(a: int16): string;
begin
  result:=FormatFloat(ctfl, a*0.01);
end;

function MavGetGPSdata(data: TMAVmessage; pos: integer;
                     var lat, lon, alt: single): boolean;  {Get lat, lon and alt from GPS data}
var
  la, lo: int32;

begin
  result:=false;
  la:=MavGetInt32(data, pos);
  lo:=MavGetInt32(data, pos+4);
  lat:=la/10000000;
  lon:=lo/10000000;
  alt:=MavGetFloatFromBuf(data, pos+8);
  if (la<>0) or (lo<>0) then
    result:=true;
end;

procedure MavGetInt123(data: TMAVmessage; pos: integer; var v1, v2, v3: int16);
begin
  v1:=MavGetInt16(data, pos);
  v2:=MavGetInt16(data, pos+2);
  v3:=MavGetInt16(data, pos+4);
end;

procedure MavGetuInt123(data: TMAVmessage; pos: integer; var v1, v2, v3: uint16);
begin
  v1:=MavGetUInt16(data, pos);
  v2:=MavGetUInt16(data, pos+2);
  v3:=MavGetUInt16(data, pos+4);
end;

function  VoltToStr(v: byte): string;
begin
  result:=FormatFloat(dzfl, v*0.1+5)+'V';
end;

function MavGetChValue(data: TMAVmessage; chnr: byte; pos: integer=27): uint16; {Channel no from 1..12 or 1..24}
var
  n: byte;

begin
  n:=((chnr-1) div 2)*3+pos;
  if (chnr and 1)=0 then begin                           {even channel no Ch0...}
    result:=lo(data.msgbytes[n+1])*256+data.msgbytes[n+2];
  end else begin                                         {uneven channel no Ch1...}
    result:=data.msgbytes[n]*16+hi(data.msgbytes[n+1]);
  end;
end;

function  GPSaccHToStr(const g: byte): string;
begin
  result:=FormatFloat(dzfl, g/20);
end;

function GetSerialNumber(const msg: TMAVmessage; pos: byte): string;
begin
  result:='';
  result:=IntToHex(MavGetUInt32(msg, pos), 8)+'-'+
          IntToHex(MavGetUInt32(msg, pos+4), 8)+'-'+
          IntToHex(MavGetUInt32(msg, pos+8), 8);
end;

function GetSystemTime(const data: TMAVmessage; pos: integer; var time: TDateTime): string;
begin
  time:=UNIXtoDateTime(MavGetUInt64(data, pos) div 1000000);      {µs}
  result:=FormatDateTime('YYYY-MM-DD hh:nn:ss', time);
end;

{Message Struktur:
 https://github.com/mavlink/c_library_v2/tree/master/common
      inconsistent !
 https://github.com/YUNEEC/MavlinkLib/blob/master/message_definitions/common.xml
 https://github.com/YUNEEC/MavlinkLib}

function MsgIDtoStr(id: uint32): string;
begin
  result:=rsUnknown_+' MAV_CMD'+' $'+IntToHex(id, 2)+
          ' ('+IntToStr(id)+')';                   {default}
  case id of
      0:  result:='heartbeat';                     {Supported Msg Länge 9}
      1:  result:='sys_status';                    {Supported Msg Länge 1F}
      2:  result:='system_time';                   {Länge 0B}
      4:  result:='ping';
      5:  result:='change_operator_control';
      6:  result:='change_operator_control_ack';
      7:  result:='auth_key';
      8:  result:='link_node_status';
     11:  result:='set_mode';
     19:  result:='param_ack_transaction';
     20:  result:='param_request_read';
     21:  result:='param_request_list';
     22:  result:='param_value';
     23:  result:='param_set';
     24:  result:='gps_raw_int';                   {Supported Msg Länge 31/32}
     25:  result:='gps_status';                    {Länge 1}
     26:  result:='scaled_imu';
    $1B:  result:='raw_imu';
    $1C:  result:='raw_pressure';
    $1D:  result:='scaled_pressure';
    $1E:  result:='attitude';                      {Länge 1C}
    $1F:  result:='attitude_quaternion';           {Supported Msg Länge 20}
    $20:  result:='local_position_ned';            {Länge 1C}
    $21:  result:='global_position_int';           {Supported Msg Länge 1C}
    $22:  result:='rc_channels_scaled';
    $23:  result:='rc_channels_raw';
    $24:  result:='servo_output_raw';              {Länge 10 oder 15}
    $25:  result:='mission_request_partial_list';
    $26:  result:='mission_write_partial_list';
    $27:  result:='mission_item';
    $28:  result:='mission_request';
    $29:  result:='mission_set_current';
    $2A:  result:='mission_current';
     43:  result:='mission_request_list';
    $2C:  result:='mission_count';                 {Länge 3 oder 5}
    $2D:  result:='mission_clear_all';
    $2E:  result:='mission_item_reached';
    $2F:  result:='mission_ack';
    $30:  result:='set_gps_global_origin';
    $31:  result:='gps_global_origin';
    $32:  result:='param_map_rc';
     51:  result:='mission_request_int';
     52:  result:='mission_changed';

     54:  result:='safety_set_allowed_area';
     55:  result:='safety_allowed_area';
     56:  result:='SERIAL_NUMBER';
    $3D:  result:='attitude_quaternion_cov';
    $3E:  result:='nav_controller_output';
    $3F:  result:='global_position_int_cov';
    $40:  result:='local_position_ned_cov';
    $41:  result:='rc_channels';                   {Supported Msg Länge 2A}
    $42:  result:='request_data_stream';
    $43:  result:='data_stream';
    $45:  result:='manual_control';                {Länge 0B}
    $46:  result:='rc_channels_override';          {Länge 11}
    $49:  result:='mission_item_int';
    $4A:  result:='vfr_hud';                       {Länge 11}
    $4B:  result:='command_int';
    $4C:  result:='command_long';                  {Länge 20}
    $4D:  result:='command_ack';
    $4E:  result:='command_cancel';                {78: UTC time stamp, Boot time}
    $4F:  result:='command_long_stamped';          {79: not supported anymore}
    $51:  result:='manual_setpoint';
    $52:  result:='set_attitude_target';
    $53:  result:='attitude_target';               {Länge 24}
    $54:  result:='set_position_target_local_ned';
    $55:  result:='position_target_local_ned';     {Länge 33}
    $56:  result:='set_position_target_global_int';
    $57:  result:='position_target_global_int';    {Länge 3}
    $59:  result:='local_position_ned_system_global_offset';
    $5A:  result:='hil_state';
    $5B:  result:='hil_controls';
    $5C:  result:='hil_rc_inputs_raw';
    $5D:  result:='hil_actuator_controls';

    $64:  result:='optical_flow';
    $65:  result:='global_vision_position_estimate';
    $66:  result:='vision_position_estimate';
    $67:  result:='vision_speed_estimate';
    $68:  result:='vicon_position_estimate';
    $69:  result:='highres_imu';                   {Länge 3E}
    $6A:  result:='optical_flow_rad';
    $6B:  result:='hil_sensor';
    $6C:  result:='sim_state';
    $6D:  result:='radio_status';
    $6E:  result:='file_transfer_protocol';
    $6F:  result:='timesync';                      {Länge 0D}
    $70:  result:='camera_trigger';
    $71:  result:='hil_gps';
    $72:  result:='hil_optical_flow';
    $73:  result:='hil_state_quaternion';
    116:  result:='scaled_imu2';
    $75:  result:='log_request_list';
    $76:  result:='log_entry';
    $77:  result:='log_request_data';
    $78:  result:='log_data';
    $79:  result:='log_erase';
    $7A:  result:='log_request_end';
    $7B:  result:='gps_inject_data';
    $7C:  result:='gps2_raw';
    $7D:  result:='power_status';
    $7E:  result:='serial_control';
    $7F:  result:='gps_rtk';
    $80:  result:='gps2_rtk';
    $81:  result:='scaled_imu3';
    $82:  result:='data_transmission_handshake';
    $83:  result:='encapsulated_data';
    $84:  result:='distance_sensor';
    $85:  result:='terrain_request';
    $86:  result:='terrain_data';
    $87:  result:='terrain_check';
    $88:  result:='terrain_report';
    $89:  result:='scaled_pressure2';
    $8A:  result:='att_pos_mocap';
    $8B:  result:='set_actuator_control_target';
    $8C:  result:='actuator_control_target';       {Länge 14}
    $8D:  result:='altitude';                      {Länge 20}
    $8E:  result:='resource_request';
    $8F:  result:='scaled_pressure3';
    $90:  result:='follow_target';
    $92:  result:='control_system_state';
    $93:  result:='battery_status';
    $94:  result:='autopilot_version';             {Länge 34, 48, 4C}
    149:  result:='landing_target';

    150: result:='SENSOR_OFFSETS';
    162:  result:='fence_status';
    163: result:='AHRS';                           {Attitude and Heading Reference System}
    165: result:='HW_STATUS';
    172: result:='DATA96';                         {Whatever this is...}
    173: result:='RANGEFINDER';
    178: result:='AHRS2';
    192:  result:='mag_cal_report';
    193: result:='EKF_STATUS_REPORT';              {Extended Kalman Filter}

    225:  result:='efi_status';

{MESSAGE IDs 180 - 229: Space for custom messages in
 individual projectname_messages.xml files -->}
(*  201:  result:='sens_power';                    {I do not know if used}
    202:  result:='sens_MPTT';
    203:  result:='aslctrl_data';
    204:  result:='aslctrl_debug';
    205:  result:='asluav_status';
    206:  result:='ekf_ext';                       {Wind speed and such stuff}
    207:  result:='asl_obctrl';
    208:  result:='sens_atmos';                    {Atmospheric sensors}
    209:  result:='sens_batmon';                   {Battery monitor}
    210:  result:='fw_soaring_data';               {fixed wing...}
    211:  result:='sensorpod_status';
    212:  result:='sens_power_board';
    213:  result:='gsm_link_status';               {LTE too}       *)

    230:  result:='estimator_status';              {Länge 2A}
    $E7:  result:='wind_cov';                      {Länge 20}
    $E8:  result:='gps_input';
    $E9:  result:='gps_rtcm_data';
    $EA:  result:='high_latency';
    $EB:  result:='high_latency2';
    241:  result:='vibration';                     {Länge 14}
    $F2:  result:='home_position';                 {Supported Msg Länge 28 oder 3C}
    $F3:  result:='set_home_position';
    $F4:  result:='message_interval';
    $F5:  result:='extended_sys_state';            {Länge 02}
    $F6:  result:='adsb_vehicle';
    $F7:  result:='collision';
    $F8:  result:='v2_extension';
    $F9:  result:='memory_vect';
    $FA:  result:='debug_vect';
    $FB:  result:='named_value_float';
    $FC:  result:='named_value_int';
    $FD:  result:='statustext';                    {Länge variabel}
    $FE:  result:='debug';
    256:  result:='setup_signing';
    $101: result:='button_change';
    $102: result:='play_tune';
    $103: result:='camera_information';
    $104: result:='camera_settings';
    $105: result:='storage_information';
    $106: result:='camera_capture_status';
    $107: result:='camera_image_captured';         {Länge FC}
    $108: result:='flight_information';            {Supported Msg Länge 1B}
    $109: result:='mount_orientation';             {Länge 20}
    $10A: result:='logging_data';
    $10B: result:='logging_data_acked';
    $10C: result:='logging_ack';
    $10D: result:='video_stream_information';
    $10E: result:='video_stream_status';           {270 len 19}
    $10F: result:='camera_fov_status';             {271 len 52}

    275:  result:='camera_tracking_image_status';  {275 len 31}
    276:  result:='camera_tracking_geo_status';    {276 len 49}

    280:  result:='gimbal_manager_information';
    281:  result:='gimbal_manager_status';
    282:  result:='gimbal_manager_set_attitude';
    283:  result:='gimbal_device_information';
    284:  result:='gimbal_device_set_attitude';
    285:  result:='gimbal_device_attitude_status';
    286:  result:='autopilot_state_for_gimbal_device';
    287:  result:='gimbal_manager_set_pitchyaw';
    288:  result:='gimbal_manager_set_manual_control';
    290:  result:='esc_info';
    291:  result:='esc_status';

    299:  result:='wifi_config_ap';
    300:  result:='protocol_version';              {12C'h not supported anymore}

    301:  result:='ais_vessel';

    310:  result:='uavcan_node_status';
    $137: result:='uavcan_node_info';
    $140: result:='param_ext_request_read';
    $141: result:='param_ext_request_list';
    $142: result:='param_ext_value';               {Länge 95}
    $143: result:='param_ext_set';
    $144: result:='param_ext_ack';                 {Länge 91}
    $14A: result:='obstacle_distance';             {Länge 9E}
    $14B: result:='odometry';
    $14C: result:='trajectory_representation_waypoints';
    $14D: result:='trajectory_representation_bezier';

    336:  result:='cellular_config';

    339:  result:='raw_rpm';
    340:  result:='UTM_global_position';           {154'h}
    350:  result:='debug_float_array';
    360:  result:='orbit_execution_status';

    370:  result:='smart_battery_info';
    373:  result:='generator_status';
    375:  result:='actuator_output_status';
    380:  result:='time_estimate_to_target';
    385:  result:='tunnel';
    390:  result:='onboard_computer_status';
    395:  result:='component_information';
    400:  result:='play_tune v2';
    401:  result:='supported_tunes';

    9000: result:='wheel_distance';                {2328'h}
    9005: result:='winch_status';

   12900: result:='open_drone_id_basic_id';        {3264'h}
   12901: result:='open_drone_id_location';
   12902: result:='open_drone_id_authentication';
   12903: result:='open_drone_id_self_id';
   12904: result:='open_drone_id_system';
   12905: result:='open_drone_id_operator_id';
   12915: result:='open_drone_id_message_pack';
   12918: result:='open_drone_id_arm_status';
   12919: result:='open_drone_id_system_update';
   19920: result:='hygrometer_sensor';
  end;
end;


end.

