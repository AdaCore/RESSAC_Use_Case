package Types with SPARK_Mode is

   ----------------------------------
   -- Types for inputs and outputs --
   ----------------------------------

   type Distance_Input_Type is range 1 .. 100; -- in n.m

   type Speed_Input_Type is range 1 .. 250; -- in k.t

   type Altitude_Input_Type is range -500 .. 3_000; -- in ft

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

   package Mks is

      type Mks_Type is new Long_Float
        with
          Dimension_System => (
           (Unit_Name => Meter,    Unit_Symbol => 'm',   Dim_Symbol => 'L'),
           (Unit_Name => Kilogram, Unit_Symbol => "kg",  Dim_Symbol => 'M'),
           (Unit_Name => Second,   Unit_Symbol => 's',   Dim_Symbol => 'T'),
           (Unit_Name => Ampere,   Unit_Symbol => 'A',   Dim_Symbol => 'I'),
           (Unit_Name => Kelvin,   Unit_Symbol => 'K',   Dim_Symbol => '@'),
           (Unit_Name => Mole,     Unit_Symbol => "mol", Dim_Symbol => 'N'),
           (Unit_Name => Candela,  Unit_Symbol => "cd",  Dim_Symbol => 'J'));

      --  SI Base dimensioned subtypes

      subtype Length is Mks_Type
        with
          Dimension => (Symbol => 'm',
                        Meter  => 1,
                        others => 0);

      subtype Time is Mks_Type
        with
          Dimension => (Symbol => 's',
                        Second => 1,
                        others => 0);

      subtype Speed is Mks_Type
        with
          Dimension => (Meter  =>  1,
                        Second => -1,
                        others =>  0);

      subtype Angle is Mks_Type
        with
          Dimension => (Symbol => "rad",
                        others => 0);

      pragma Warnings (Off, "*assumed to be*");
      m   : constant Length := 1.0;
      s   : constant Time   := 1.0;
      pragma Warnings (On, "*assumed to be*");
   end Mks;

   use Mks;

   subtype Distance_Type is Length; -- type of P, unit and bounds ???

   subtype Speed_Type is Mks.Speed; -- type of P_Dot and Q_Dot, in angle.s-1, bounds ???

   Zero_Speed : constant Speed_Type := 0.0*m*s**(-1);

   subtype Angle_Type is Angle; -- type of Q, unit and bounds ???

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
