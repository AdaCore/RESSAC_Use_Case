--  This package provides a wrapper above MMS.F_PT.F_MM.Behavior.Run which
--  is used to verify in SPARK that high level guarantees on F_MM are implied
--  by its behavioural specification.

with Types; use Types;

package MMS.F_PT.F_MM.Behavior.Guarantees with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);

   -----------------------------------
   -- High-Level Properties on F_MM --
   -----------------------------------

   function In_Take_Off_State return Boolean is
      (Power_State = On
       and then On_State = RUNNING
       and then Running_State = TAKE_OFF);

   function Mission_Aborted return Boolean is
      (Power_State = On
       and then On_State = ABORTED);

   function Mission_Cancelled return Boolean is
      (Power_State = On
       and then On_State = INIT
       and then Init_State = CANCELLED)
   with Global => Private_State;

   -----------------------------------
   -- High-Level Garantees for F_MM --
   -----------------------------------

   procedure Run with
     Post =>

      --  6.6.3.A Viability guarantee: no take-off if energy aboard is
      --  incompatible with mission completion.

      (if In_Take_Off_State and then not In_Take_Off_State'Old then
         Initial_Energy_Compatible_With_Mission)

      --  6.6.3.B Any mission cancellation is signaled to CP and GS.

      and then
       (if Mission_Aborted and then not Mission_Aborted'Old then
           Mission_Aborted_Signaled)
      and then
       (if Mission_Cancelled and then not Mission_Cancelled'Old then
           Mission_Cancelled_Signaled)

      --  6.6.3.2.A Missions cancelled for energy reasons can be proven
      --  infeasible.

      and then
       (if Mission_Aborted and then not Mission_Aborted'Old
          and then Aborted_For_Energy_Reasons
        then not In_Flight_Energy_Compatible_With_Mission);

end MMS.F_PT.F_MM.Behavior.Guarantees;
