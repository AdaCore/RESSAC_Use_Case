with System.Dim.Mks; use System.Dim; use all type System.Dim.Mks.Mks_Type;

package Types is

   ----------------------------------
   -- Types for inputs and outputs --
   ----------------------------------

   type Distance_Input_Type is range 1 .. 100; -- in n.m

   type Speed_Input_Type is range 1 .. 250; -- in k.t

   type Altitude_Input_Type is range -500 .. 3000; -- in ft

   type Navigation_Parameters_Type is record
      Distance : Distance_Input_Type;
      Speed    : Speed_Input_Type;
      Altitude : Altitude_Input_Type;
   end record;

   type Navigation_Parameters_Type_Option (Present : Boolean := False) is record
      case Present is
      when True  =>
         Content : Navigation_Parameters_Type;
      when False =>
         null;
      end case;
   end record;

   type Navigation_Mode_Type is (RP, A);

   type Navigation_Mode_Type_Option (Present : Boolean := False) is record
      case Present is
      when True  =>
         Content : Navigation_Mode_Type;
      when False =>
         null;
      end case;
   end record;

   type Navigation_Option_Type is (SPEED, ALTITUDE, ENERGY);

   type Navigation_Option_Type_Option (Present : Boolean := False) is record
      case Present is
      when True  =>
         Content : Navigation_Option_Type;
      when False =>
         null;
      end case;
   end record;

   type USB_Key_Type is record
      Navigation_Parameters : Navigation_Parameters_Type;
      Navigation_Option     : Navigation_Option_Type;
   end record;

   type USB_Key_Type_Option (Present : Boolean := False) is record
      case Present is
      when True  =>
         Content : USB_Key_Type;
      when False =>
         null;
      end case;
   end record;

   type Bay_Switch_Type is (OPEN, CLOSED);

   type Payload_Mass_Type is new Integer range 0 .. 98; -- in kg

   subtype Distance_Type is Mks.Length; -- type of P, unit and bounds ???

   subtype Speed_Type is Mks.Speed; -- type of P_Dot and Q_Dot, in angle.s-1, bounds ???

   Zero_Speed : constant Speed_Type := 0.0*Mks.m*Mks.s**(-1);

   subtype Angle_Type is Mks.Angle; -- type of Q, unit and bounds ???

   type Rotactor_Type is range 0 .. 9;

   type CP_Switches_Type is record
      Power      : Boolean;
      Mode       : Navigation_Mode_Type;
      Bay        : Bay_Switch_Type;
      Start      : Boolean;
      Rotactor_1 : Rotactor_Type;
      Rotactor_2 : Rotactor_Type;
   end record;

   type Source_Type is range 1 .. 100;

   type CP_Displays_Type is record
      Ready            : Boolean;
      Cancelled        : Boolean;
      Complete         : Boolean;
      Aborted          : Boolean; -- ???
      Primary_Source   : Source_Type;
      Secondary_Source : Source_Type;
   end record;

   type Propulsion_Energy_Type is record
      Primary_Source_Capacity   : Source_Type;
      Secondary_Source_Capacity : Source_Type;
   end record;

   type Torque_Type is delta 0.0001 range -10.0E6 .. 10.0E6; -- ???
end Types;
