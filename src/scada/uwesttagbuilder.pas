{$i ../common/language.inc}
{$IFDEF PORTUGUES}
{:
Unit que implementa o West 6100+ TagBuilder assistente.
@author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ELSE}
{:
Unit that implements the West 6100+ TagBuilder wizard.
@author(Fabio Luis Girardi <fabio@pascalscada.com>)
}
{$ENDIF}
unit uwesttagbuilder;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources,{$ENDIF}SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Spin, ExtCtrls;

type
  TVarItem = record
    Enabled:TCheckBox;
    TagName:TEdit;
    Scan:TSpinEdit;
  end;

  TWestTagBuilder = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    AdrStart: TSpinEdit;
    Label2: TLabel;
    AdrEnd: TSpinEdit;
    ZeroFill: TCheckBox;
    ScrollBox1: TScrollBox;
    Panel2: TPanel;
    Panel3: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure OnTagNameExit(Sender: TObject);
    procedure OnTagNameEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    VarDesc,TagNames, OldTagName:array[$00..$1B] of string;
  public
    Variaveis:array[$00..$1B] of TVarItem;
    destructor Destroy; override;
  end;

var
  WestTagBuilder: TWestTagBuilder;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$IF defined(FPC) AND (FPC_FULLVERSION >= 20400) }
    {$R uwesttagbuilder.lfm}
  {$IFEND}
{$ENDIF}

uses hsstrings;

procedure TWestTagBuilder.Button1Click(Sender: TObject);
var
  c:Integer;
  atleastOne:Boolean;
begin
  atleastOne:=false;
  for c := 0 to $1B do
    atleastOne:=atleastOne or Variaveis[c].Enabled.Checked;

  if not atleastOne then
    raise Exception.Create(SCheckAtLeastOneVariable);

end;

destructor TWestTagBuilder.Destroy;
var
  c:Integer;
begin
  for c := 0 to $1b do begin
    Variaveis[c].Enabled.Destroy;
    Variaveis[c].TagName.Destroy;
    Variaveis[c].Scan.Destroy
  end;

  inherited Destroy
end;

procedure TWestTagBuilder.FormCreate(Sender: TObject);
var
  c:Integer;
begin
  VarDesc[$00]:= 'SetPoint (SP)';
  VarDesc[$01]:= 'Process Variable (PV)';
  VarDesc[$02]:= 'Power Output value';
  VarDesc[$03]:= 'Controller status';
  VarDesc[$04]:= 'Scale Range Max';
  VarDesc[$05]:= 'Scale Range Min';
  VarDesc[$06]:= 'Scale Range Dec. Point';
  VarDesc[$07]:= 'Input filter time constant';
  VarDesc[$08]:= 'Output 1 Power Limit';
  VarDesc[$09]:= 'Output 1 cycle time';
  VarDesc[$0A]:= 'Output 2 cycle time';
  VarDesc[$0B]:= 'Recorder output scale max';
  VarDesc[$0C]:= 'Recorder output scale min';
  VarDesc[$0D]:= 'SetPoint ramp rate';
  VarDesc[$0E]:= 'Setpoint high limit';
  VarDesc[$0F]:= 'Setpoint low limit';
  VarDesc[$10]:= 'Alarm 1 value';
  VarDesc[$11]:= 'Alarm 2 value';
  VarDesc[$12]:= 'Rate (Derivative time constant)';
  VarDesc[$13]:= 'Reset (Integral time constant)';
  VarDesc[$14]:= 'Manual time reset (BIAS)';
  VarDesc[$15]:= 'ON/OFF diferential';
  VarDesc[$16]:= 'Overlap/Deadband';
  VarDesc[$17]:= 'Proportional band 1 value';
  VarDesc[$18]:= 'Proportional band 2 value';
  VarDesc[$19]:= 'PV Offset';
  VarDesc[$1A]:= 'Arithmetic deviation';
  VarDesc[$1B]:= 'Arithmetic deviation'; // checar estas descrições.

  TagNames[$00]:='WEST%a_SP';
  TagNames[$01]:='WEST%a_PV';
  TagNames[$02]:='WEST%a_Power_Output_value';
  TagNames[$03]:='WEST%a_Controller_status';
  TagNames[$04]:='WEST%a_Scale_Range_Max';
  TagNames[$05]:='WEST%a_Scale_Range_Min';
  TagNames[$06]:='WEST%a_Scale_Range_Decimal_Point';
  TagNames[$07]:='WEST%a_Input_filter_time_constant';
  TagNames[$08]:='WEST%a_Output_1_Power_Limit';
  TagNames[$09]:='WEST%a_Output_1_cycle_time';
  TagNames[$0A]:='WEST%a_Output_2_cycle_time';
  TagNames[$0B]:='WEST%a_Recorder_output_scale_max';
  TagNames[$0C]:='WEST%a_Recorder_output_scale_min';
  TagNames[$0D]:='WEST%a_SetPoint_ramp_rate';
  TagNames[$0E]:='WEST%a_Setpoint_high_limit';
  TagNames[$0F]:='WEST%a_Setpoint_low_limit';
  TagNames[$10]:='WEST%a_Alarm_1_value';
  TagNames[$11]:='WEST%a_Alarm_2_value';
  TagNames[$12]:='WEST%a_Derivative_time_constant';
  TagNames[$13]:='WEST%a_Integral_time_constant';
  TagNames[$14]:='WEST%a_Manual_time_reset';
  TagNames[$15]:='WEST%a_ON_OFF_diferential';
  TagNames[$16]:='WEST%a_Overlap_Deadband';
  TagNames[$17]:='WEST%a_Proportional_band_1_value';
  TagNames[$18]:='WEST%a_Proportional_band_2_value';
  TagNames[$19]:='WEST%a_PV_Offset';
  TagNames[$1A]:='WEST%a_Arithmetic_deviation1';
  TagNames[$1B]:='WEST%a_Arithmetic_deviation2'; //_checar_estas_descrições.

  ScrollBox1.Visible:=false;

  for c := 0 to $1b do begin
    Variaveis[c].Enabled:=TCheckBox.Create(Self);
    Variaveis[c].Enabled.Parent:=ScrollBox1;
    Variaveis[c].Enabled.Left := 5;
    Variaveis[c].Enabled.Top  := 4 + (c*24);
    Variaveis[c].Enabled.Width:= 186;
    Variaveis[c].Enabled.Caption := VarDesc[c];

    Variaveis[c].TagName:=TEdit.Create(Self);
    Variaveis[c].TagName.Parent:=ScrollBox1;
    Variaveis[c].TagName.Left   := 196;
    Variaveis[c].TagName.Top    := 2 + (c*24);
    Variaveis[c].TagName.Height := 22;
    Variaveis[c].TagName.Width  := 202;
    Variaveis[c].TagName.Text   := TagNames[c];
    Variaveis[c].TagName.Tag    := c;

    Variaveis[c].Scan:=TSpinEdit.Create(Self);
    Variaveis[c].Scan.Parent:=ScrollBox1;
    Variaveis[c].Scan.Left     := 402;
    Variaveis[c].Scan.Top      := 2 + (c*24);
    Variaveis[c].Scan.Height   := 22;
    Variaveis[c].Scan.Width    := 84;
    Variaveis[c].Scan.MinValue := 1; //1 milisegundo
    Variaveis[c].Scan.MaxValue := 7200000; //2 horas máximo
    Variaveis[c].Scan.Value    := 1000; //1 segundo
  end;

  ScrollBox1.Visible:=true;  
end;

procedure TWestTagBuilder.OnTagNameEnter(Sender: TObject);
begin
 if Sender is TEdit then
   with Sender as TEdit do begin
     OldTagName[Tag]:=Text;
   end;
end;

procedure TWestTagBuilder.OnTagNameExit(Sender: TObject);
var
  c:Integer;
begin
 if Sender is TEdit then
   with Sender as TEdit do begin
     if (trim(Text)='') or (not (Text[1] in ['A'..'Z','a'..'z','_'])) then begin
       Text := OldTagName[Tag];
       exit;
     end;
     
     for c := 0 to $1b do
       if Tag<>c then
         if Text=Variaveis[c].TagName.Text then begin
           Text := OldTagName[Tag];
           exit;
         end;
   end;
end;

{$IFDEF FPC }
  {$IF defined(FPC) AND (FPC_FULLVERSION < 20400) }
initialization
  {$i uwesttagbuilder.lrs}
  {$IFEND}
{$ENDIF}

end.