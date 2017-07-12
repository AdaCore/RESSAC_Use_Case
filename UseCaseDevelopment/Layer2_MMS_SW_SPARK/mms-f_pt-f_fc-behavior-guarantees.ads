with MMS.F_PT.F_FC.Data;
with Types; use Types;

package MMS.F_PT.F_FC.Behavior.Guarantees with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);

   -----------------------------------
   -- High-Level Properties on F_FC --
   -----------------------------------

   subtype Propulsion_State_Type is Engine_State_Type
   range PROPULSION .. WAITING_BRAK;

   subtype Braking_State_Type is Engine_State_Type
   range BRAKING .. WAITING_PROP;

   function Engine_State_In_Braking return Boolean is
     (On_State = RUNNING
      and then Engine_State in Braking_State_Type);

   function Engine_State_In_Propulsion return Boolean is
     (On_State = RUNNING
      and then Engine_State in Propulsion_State_Type);

   function Get_Time_Since_In_Safety_Escape return Time_Type with
     Ghost;

   -----------------------------------
   -- High-Level Garantees for F_FC --
   -----------------------------------

   procedure Run with
     Post =>

     --  6.7.3.2.D Propulsion and braking torque actions are in mutual
     --  exclusion.

     (if (Engine_State_In_Propulsion'Old and then Engine_State_In_Braking)
      or else (Engine_State_In_Braking'Old and then Engine_State_In_Propulsion)
      then Time_Since_Stopped > MMS.F_PT.F_FC.Data.Commutation_Duration)

     --  6.7.3.2.E In-flight mission concellation with remaining propulsion
     --  capacity implies occurrence of safety excapes for more than
     --  Escape_Time seconds.

     and then
       (if On_State = ABORTED and then On_State'Old /= ABORTED
           and then Aborted_With_Propulsion_Available
        then Get_Time_Since_In_Safety_Escape > MMS.F_PT.F_FC.Data.Escape_Time);

end MMS.F_PT.F_FC.Behavior.Guarantees;
