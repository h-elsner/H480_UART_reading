unit mav1defs;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

const
  dzfl='0.0';                                      {Formatierung für FormatFloat}
  ctfl='0.00';
  mlfl='0.000';
  coordfl8='0.00000000';

  v1magic='0xFE';
  v1magicbyte=$FE;
  BCmagic=$BC;
  NumPayloadBytes=255;       {reserved lenght for data}
  lenfix=10;                 {+ fix part}

  rsUnknown_='Unknown_';
  rsUndef_='Undef_';


type
  TMAVmessage = record
    timepoint: TDateTime;
    msglength: integer;
    msgbytes: array[0..NumPayLoadBytes+lenfix] of byte;
    sysid: byte;
    targetid: byte;
    msgid: byte;
    valid: boolean;
  end;

{Public functions and procedures}

function ClearMAVmessage: TMAVmessage;
function MsgIDToStr(id: byte): string;
function SysIDToStr(id: byte): string;
function TargetIDToStr(id: byte): string;
function SensorTypeToStr(id: byte): string;  {Message BC}
function IntToYaw(v: integer): string;
function IntToAngle(v: integer): string;
function IntToPan(v: integer): string;
function IntToTilt(v: integer): string;
function IntToPanMode(v: integer): string;
function IntToTiltMode(v: integer): string;
function IntToLatLon(v: integer): string;
function FloatToAlt(v: single): string;
function IntToAlt(v: integer): string;
function IntToAccuracy(v: integer): string;
function IntToRCAngle(v: integer): string;
function IntToSpeed(v: integer): string;

implementation

function ClearMAVmessage: TMAVmessage;
var
  i: integer;

begin
  result.msglength:=0;
  result.timepoint:=0;
  result.sysid:=0;
  result.targetid:=0;
  result.valid:=false;
  for i:=0 to lenfix do
    result.msgbytes[i]:=0;                    {Fix part empty}
end;

function MsgIDToStr(id: byte): string;
begin
  result:=rsUnknown_+' '+IntToStr(id)+' (0x'+HexStr(id, 2)+')';
  case id of
    0: result:='HEARTBEAT';                   {1Hz}
    1: result:='Attitude';                    {Frequently}
    2: result:='Telemetry_5GHz';
    3: result:='Gimbal position';             {Sys_Id Camera?; 1Hz }
    8: result:='ChannelData_5GHz';
    20: result:='PARAM_REQUEST_READ';
    76: result:='Answer param request';
    255: result:='Sensor data';
  end;
end;

function SysIDToStr(id: byte): string;        {Sent from}
begin
  result:=rsUnknown_+' '+IntToStr(id)+' (0x'+HexStr(id, 2)+')';
  case id of
    1: result:='Autopilot';                   {Flight controller (Autopilot)}
    2: result:='Camera';
    3, 200: result:='Gimbal';                 {200 in nested Sensor data}
    4: result:='Remote';
    6: result:='SysID6';
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
    99: result:='Remote';                    {Platzhalter}
  end;
end;

function SensorTypeToStr(id: byte): string;  {Message BC}
begin
  result:=rsUnknown_+' '+IntToStr(id)+' (0x'+HexStr(id, 2)+')';
  case id of
    24:  result:='GPS data (0x18=24)';
    178: result:='Home? (0xB2=178)';
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
    2184: result:='Angle mode';                 {+10%}
    3412: result:='Velocity mode';              {+100%}
  end;
end;

function IntToPanMode(v: integer): string;
begin
  result:=rsUndef_+IntToStr(v);
  case v of
    683: result:='Follow mode';                 {-100%}
    1433: result:='Team mode';                  {-45%}
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
    0: result:=   '-150% (00 00)';
    683: result:= '-100% (AB 02)';
    1433: result:='-45% (00 05)';
    1502: result:='-40% (DE 05)';
    2048: result:='0% (00 08)';
    2184: result:='+10% (88 08)';
    3412: result:='+100% (54 0D)';
    4095: result:='+150% (FF 0F)';
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

function IntToRCAngle(v: integer): string;
begin
  result:=FormatFloat(dzfl, v*0.01);
end;

function IntToSpeed(v: integer): string;
begin
  result:=FormatFloat(dzfl, v*0.1);
end;

end.

