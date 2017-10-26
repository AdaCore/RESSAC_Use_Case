with MMS.F_PT.F_FC.State;

package body MMS.F_PT.F_FC.Behavior.Guarantees with SPARK_Mode is

   Stored_Time_Since_In_Safety_Escape : Time_Type with Ghost;
   function Get_Time_Since_In_Safety_Escape return Time_Type is
     (Stored_Time_Since_In_Safety_Escape);

   procedure Run is
   begin
      if On_State = RUNNING then
         Check_Safety_Escape;
         if not In_Safety_Envelope then
            Stored_Time_Since_In_Safety_Escape := Time_Since_In_Safety_Escape;
         end if;

         Choose_Operating_Mode;
      end if;

      AV_State_Vector;
      Update_State;

      if On_State = RUNNING then
         Propulsion_Braking_Mutual_Exclusion;

         Reference_Trajectory_Computation;

         if Engine_State = PROPULSION then
            Gain_Scheduling;
            Propulsion_Control;
            State.Braking_Torque := 0.0;
         elsif Engine_State = BRAKING then
            Gain_Scheduling;
            Braking_Control;
            State.Propulsion_Torque := 0.0;
         else
            State.Braking_Torque := 0.0;
            State.Propulsion_Torque := 0.0;
         end if;
      end if;
   end Run;

end MMS.F_PT.F_FC.Behavior.Guarantees;
