with MMS.F_PT.Data;

with Types; use Types;

package MMS.F_PT.F_FC.Data with SPARK_Mode is

   --  ??? Types need to be precisely defined.

   ------------
   -- Tables --
   ------------

   --  From 6.7.2.3

   Flight_Domain_Mesh : constant Flight_Domain_Mesh_Type (1 .. 100, 1 .. 100); -- ??? bounds

   function Climb_Gains
     (S : Flight_Speed_Center;
      A : Flight_Altitude_Center;
      M : Payload_Mass_Center) return Gain_Triple
   with
     Pre => S in Flight_Domain_Mesh'Range (1)
     and then A in Flight_Domain_Mesh'Range (2)
     and then M in MMS.F_PT.Data.Payload_Mass_Grid'Range;

   function Cruise_Gains
     (S : Flight_Speed_Center;
      A : Flight_Altitude_Center;
      M : Payload_Mass_Center) return Gain_Triple
   with
     Pre => S in Flight_Domain_Mesh'Range (1)
     and then A in Flight_Domain_Mesh'Range (2)
     and then M in MMS.F_PT.Data.Payload_Mass_Grid'Range;

   function Descent_Gains
     (S : Flight_Speed_Center;
      A : Flight_Altitude_Center;
      M : Payload_Mass_Center) return Gain_Triple
   with
     Pre => S in Flight_Domain_Mesh'Range (1)
     and then A in Flight_Domain_Mesh'Range (2)
     and then M in MMS.F_PT.Data.Payload_Mass_Grid'Range;

   ---------------
   -- Constants --
   ---------------

   --  From 6.7.3.2

   Qdot_MinCl : constant Speed_Type; --  in angle.s-1
   Qdot_MaxCl : constant Speed_Type; --  in angle.s-1
   Q_MaxCl    : constant Angle_Type; --  in angle
   Qdot_MinCr : constant Speed_Type; --  in angle.s-1
   Qdot_MaxCr : constant Speed_Type; --  in angle.s-1
   Q_MinCr    : constant Angle_Type; --  in angle
   Pdot_MaxCr : constant Speed_Type; --  in angle.s-1
   Qdot_MinDs : constant Speed_Type; --  in angle.s-1
   Qdot_MaxDs : constant Speed_Type; --  in angle.s-1
   Q_MaxDs    : constant Angle_Type; --  in angle

   Escape_Time : constant Time_Type; --  in s

   --  From 6.7.4

   Commutation_Duration : constant Time_Type; --  in s
   Hazard_Duration      : constant Time_Type; --  in s

   Recovery_Speed : constant Current_Speed_Type; --  in m.s

   J0 : constant Integer; --  in kg.m2
   L  : constant Integer; --  in m
   M0 : constant Integer; --  in kg

private
   pragma SPARK_Mode (Off);

   Flight_Domain_Mesh : constant Flight_Domain_Mesh_Type (1 .. 100, 1 .. 100) :=
     (others => (others => <>));

   Qdot_MinCl : constant Speed_Type := 0.0;
   Qdot_MaxCl : constant Speed_Type := 0.0;
   Q_MaxCl    : constant Angle_Type := 0.0;
   Qdot_MinCr : constant Speed_Type := 0.0;
   Qdot_MaxCr : constant Speed_Type := 0.0;
   Q_MinCr    : constant Angle_Type := 0.0;
   Pdot_MaxCr : constant Speed_Type := 0.0;
   Qdot_MinDs : constant Speed_Type := 0.0;
   Qdot_MaxDs : constant Speed_Type := 0.0;
   Q_MaxDs    : constant Angle_Type := 0.0;

   Escape_Time : constant Time_Type := 0;

   --  From 6.7.4

   Commutation_Duration : constant Time_Type := 0;
   Hazard_Duration      : constant Time_Type := 0;

   Recovery_Speed : constant Current_Speed_Type := 0;

   J0 : constant Integer := 0;
   L  : constant Integer := 0;
   M0 : constant Integer := 0;

end MMS.F_PT.F_FC.Data;
