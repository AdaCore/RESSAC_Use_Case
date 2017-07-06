with MMS.F_PT.F_FC.State;

package body MMS.F_PT.F_FC.Behavior.Guarantees with SPARK_Mode is

   procedure Run is
   begin
      Update_State;

      if Mission_State in FLIGHT | LANDING then
         Propulsion_Braking_Mutual_Exclusion;

         Reference_Trajectory_Computation;
         Gain_Scheduling;

         if Engine_State = PROPULSION then
            Propulsion_Control;
            State.Braking_Torque := 0.0;
         elsif Engine_State = BRAKING then
            Braking_Control;
            State.Propulsion_Torque := 0.0;
         else
            State.Braking_Torque := 0.0;
            State.Propulsion_Torque := 0.0;
         end if;
      end if;
   end Run;

end MMS.F_PT.F_FC.Behavior.Guarantees;
