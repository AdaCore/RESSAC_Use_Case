package MMS.F_PT.F_FC.Data is

   --  ??? Types need to be precisely defined.

   ------------
   -- Tables --
   ------------

   --  From 6.7.2.3

   type F_FC_Table_Type is array (Positive range <>) of Integer;

   Climb_Gains   : F_FC_Table_Type (1 .. 1);
   Cruise_Gains  : F_FC_Table_Type (1 .. 1);
   Descent_Gains : F_FC_Table_Type (1 .. 1);

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
