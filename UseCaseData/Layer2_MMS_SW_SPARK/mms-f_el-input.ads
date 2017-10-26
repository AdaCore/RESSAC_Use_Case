with MMS.Input;
with MMS.F_PT.Output;

with Types; use Types;

package MMS.F_EL.Input is

   --------------
   -- From MMS --
   --------------

   function P return Distance_Type
     renames MMS.Input.P;

   function P_Dot return Speed_Type
     renames MMS.Input.P_Dot;
   
   function Q return Angle_Type
     renames MMS.Input.Q;
   
   ---------------
   -- From F_PT --
   ---------------
   
   function Emergency_Landing return Boolean
     renames MMS.F_PT.Output.Emergency_Landing;
   
end MMS.F_EL.Input;
