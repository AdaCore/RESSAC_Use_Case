package MMS.F_PT.F_FC.Data is

   --  ??? Types need to be precisely defined.

   ------------
   -- Tables --
   ------------

   --  From 6.7.2.3

   type Gain_Type is new Integer; --  ??? some bounds

   type Gain_Triple is record
      Kd : Gain_Type;
      Kp : Gain_Type;
      Ki : Gain_Type;
   end record;

   function Climb_Gains
     (Mass     : Payload_Mass_Type;
      Altitude : Current_Altitude_Type;
      Speed    : Current_Speed_Type) return Gain_Triple;

   function Cruise_Gains
     (Mass     : Payload_Mass_Type;
      Altitude : Current_Altitude_Type;
      Speed    : Current_Speed_Type) return Gain_Triple;

   function Descent_Gains
     (Mass     : Payload_Mass_Type;
      Altitude : Current_Altitude_Type;
      Speed    : Current_Speed_Type) return Gain_Triple;

   ---------------
   -- Constants --
   ---------------

   --  From 6.7.3.2

   Qdot_MinCl : Integer; --  in angle.s-1
   Qdot_MaxCl : Integer; --  in angle.s-1
   Q_MaxCl    : Integer; --  in angle.s-1
   Qdot_MinCr : Integer; --  in angle.s-1
   Q_MinCr    : Integer; --  in angle.s-1
   Pdot_MaxCr : Integer; --  in angle.s-1
   Qdot_MinDs : Integer; --  in angle.s-1
   Qdot_MaxDs : Integer; --  in angle.s-1
   Q_MaxDs    : Integer; --  in angle.s-1

   Escape_Time : Integer; --  in s

   --  From 6.7.4

   Commutation_Duration : Integer; --  in s
   Hazard_Duration      : Integer; --  in s

   Recovery_Speed : Integer; --  in m.s

   J0 : Integer; --  in kg.m2
   L  : Integer; --  in m
   M0 : Integer; --  in kg

end MMS.F_PT.F_FC.Data;
