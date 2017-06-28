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

   Qdot_MinCl : constant Angular_Speed_Type; --  in angle.s-1
   Qdot_MaxCl : constant Angular_Speed_Type; --  in angle.s-1
   Q_MaxCl    : constant Angle_Type; --  in angle
   Qdot_MinCr : constant Angular_Speed_Type; --  in angle.s-1
   Qdot_MaxCr : constant Angular_Speed_Type; --  in angle.s-1
   Q_MinCr    : constant Angle_Type; --  in angle
   Pdot_MaxCr : constant Speed_Type; --  in km/h
   Qdot_MinDs : constant Angular_Speed_Type; --  in angle.s-1
   Qdot_MaxDs : constant Angular_Speed_Type; --  in angle.s-1
   Q_MaxDs    : constant Angle_Type; --  in angle

   Escape_Time : constant Time_Type; --  in s

   --  From 6.7.4

   Commutation_Duration : constant Time_Type; --  in s
   Hazard_Duration      : constant Time_Type; --  in s

   Recovery_Speed : constant Integer; --  in m.s

   J0 : constant Integer; --  in kg.m2
   L  : constant Integer; --  in m
   M0 : constant Integer; --  in kg

private
   pragma SPARK_Mode (Off);

   Flight_Domain_Mesh : constant Flight_Domain_Mesh_Type (1 .. 100, 1 .. 100) :=
     (others => (others => <>));

   Qdot_MinCl : constant Angular_Speed_Type := 0.0; --  in angle.s-1
   Qdot_MaxCl : constant Angular_Speed_Type := 0.0; --  in angle.s-1
   Q_MaxCl    : constant Angle_Type := 0.0; --  in angle
   Qdot_MinCr : constant Angular_Speed_Type := 0.0; --  in angle.s-1
   Qdot_MaxCr : constant Angular_Speed_Type := 0.0; --  in angle.s-1
   Q_MinCr    : constant Angle_Type := 0.0; --  in angle
   Pdot_MaxCr : constant Speed_Type := 0.0; --  in km/h
   Qdot_MinDs : constant Angular_Speed_Type := 0.0; --  in angle.s-1
   Qdot_MaxDs : constant Angular_Speed_Type := 0.0; --  in angle.s-1
   Q_MaxDs    : constant Angle_Type := 0.0; --  in angle

   Escape_Time : constant Time_Type := 0; --  in s

   --  From 6.7.4

   Commutation_Duration : constant Time_Type := 0; --  in s
   Hazard_Duration      : constant Time_Type := 0; --  in s

   Recovery_Speed : constant Integer := 0; --  in m.s

   J0 : constant Integer := 0; --  in kg.m2
   L  : constant Integer := 0; --  in m
   M0 : constant Integer := 0; --  in kg

end MMS.F_PT.F_FC.Data;
