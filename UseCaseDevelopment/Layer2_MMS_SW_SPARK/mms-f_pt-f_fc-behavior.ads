with MMS.F_PT.F_FC.Data;

with Types; use Types;

package MMS.F_PT.F_FC.Behavior is

   ------------
   -- Inputs --
   ------------

   function P return Distance_Type;
   function P_Dot return Speed_Type;
   function Q return Angle_Type;

   ----------------------
   -- Estimated Values --
   ----------------------

   function Q_Dot return Angular_Speed_Type;

   ------------
   -- States --
   ------------

   type Phase_State_Type is (CLIMB, CRUISE, DESCENT);

   function Phase_State return Phase_State_Type;

   ----------------
   -- Properties --
   ----------------

   --  From 6.7.3.2

   function In_Safety_Envelope return Boolean is
     (case Phase_State is
         when CLIMB   =>
            Q_Dot in MMS.F_PT.F_FC.Data.Qdot_MinCl .. MMS.F_PT.F_FC.Data.Qdot_MaxCl
              and Q < MMS.F_PT.F_FC.Data.Q_MaxCl,
         when CRUISE  =>
            Q_Dot in MMS.F_PT.F_FC.Data.Qdot_MinCr .. MMS.F_PT.F_FC.Data.Qdot_MaxCr
              and Q > MMS.F_PT.F_FC.Data.Q_MinCr
              and P_Dot < MMS.F_PT.F_FC.Data.Pdot_MaxCr,
         when DESCENT =>
            Q_Dot in MMS.F_PT.F_FC.Data.Qdot_MinDs .. MMS.F_PT.F_FC.Data.Qdot_MaxDs
              and Q < MMS.F_PT.F_FC.Data.Q_MaxDs);

end MMS.F_PT.F_FC.Behavior;
