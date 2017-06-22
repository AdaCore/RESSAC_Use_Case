with MMS.F_PT.F_FC.Data;

package MMS.F_PT.F_FC.Behavior is

   ------------
   -- Inputs --
   ------------

   function P return Current_Range_Type;
   function P_Dot return Current_Speed_Type;

   ----------------------
   -- Estimated Values --
   ----------------------

   function Q_Angle return Angle_Type;
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
              and Q_Angle < MMS.F_PT.F_FC.Data.Q_MaxCl,
         when CRUISE  =>
            Q_Dot in MMS.F_PT.F_FC.Data.Qdot_MinCr .. MMS.F_PT.F_FC.Data.Qdot_MaxCr
              and Q_Angle > MMS.F_PT.F_FC.Data.Q_MinCr
              and P_Dot < MMS.F_PT.F_FC.Data.Pdot_MaxCr,
         when DESCENT =>
            Q_Dot in MMS.F_PT.F_FC.Data.Qdot_MinDs .. MMS.F_PT.F_FC.Data.Qdot_MaxDs
              and Q_Angle < MMS.F_PT.F_FC.Data.Q_MaxDs);

end MMS.F_PT.F_FC.Behavior;
