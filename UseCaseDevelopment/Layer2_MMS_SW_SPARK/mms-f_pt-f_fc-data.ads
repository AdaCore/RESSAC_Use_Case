with MMS.F_PT.Data;

with Types; use Types;

package MMS.F_PT.F_FC.Data is

   --  ??? Types need to be precisely defined.

   ------------
   -- Tables --
   ------------

   --  From 6.7.2.3

   Flight_Domain_Mesh : Flight_Domain_Mesh_Type (1 .. 100, 1 .. 100); -- ??? bounds

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

   Qdot_MinCl : Angular_Speed_Type; --  in angle.s-1
   Qdot_MaxCl : Angular_Speed_Type; --  in angle.s-1
   Q_MaxCl    : Angle_Type; --  in angle
   Qdot_MinCr : Angular_Speed_Type; --  in angle.s-1
   Qdot_MaxCr : Angular_Speed_Type; --  in angle.s-1
   Q_MinCr    : Angle_Type; --  in angle
   Pdot_MaxCr : Speed_Type; --  in km/h
   Qdot_MinDs : Angular_Speed_Type; --  in angle.s-1
   Qdot_MaxDs : Angular_Speed_Type; --  in angle.s-1
   Q_MaxDs    : Angle_Type; --  in angle

   Escape_Time : Integer; --  in s

   --  From 6.7.4

   Commutation_Duration : Integer; --  in s
   Hazard_Duration      : Integer; --  in s

   Recovery_Speed : Integer; --  in m.s

   J0 : Integer; --  in kg.m2
   L  : Integer; --  in m
   M0 : Integer; --  in kg

end MMS.F_PT.F_FC.Data;
