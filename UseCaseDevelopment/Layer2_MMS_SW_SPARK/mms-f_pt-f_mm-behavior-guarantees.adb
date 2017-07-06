with Types; use Types;

package body MMS.F_PT.F_MM.Behavior.Guarantees with SPARK_Mode is

   Initial_Energy_Test_Done   : Boolean with Ghost;
   In_Flight_Energy_Test_Done : Boolean with Ghost;
   Energy_Test_Succeded       : Boolean with Ghost;

   function Initial_Energy_Test_Succeeded return Boolean is
     (Initial_Energy_Test_Done and then Energy_Test_Succeded);

   function In_Flight_Energy_Test_Failed return Boolean is
     (In_Flight_Energy_Test_Done and then not Energy_Test_Succeded);

   procedure Run is
   begin
      Initial_Energy_Test_Done := False;
      In_Flight_Energy_Test_Done := False;
      Energy_Test_Succeded := False;

      Read_Inputs;

      if Power_On then
         Management_Of_Navigation_Modes_Options_Parameters;

         if Power_State = On then
            if On_State in INIT | RUNNING
              and then Mission_Parameters_Defined
            then
               Operating_Point_Update_Management;
            end if;

            if (On_State = RUNNING
                and then Running_State = FLIGHT
                and then Current_Flight_Phase = CRUISE)
              or else
                (On_State = INIT
                 and then Init_Completed)
            then
               Mission_Viability_Logic;

               if On_State = RUNNING then
                  In_Flight_Mission_Viability_Logic;

                  In_Flight_Energy_Test_Done := True;
                  Energy_Test_Succeded :=
                    In_Flight_Energy_Compatible_With_Mission;

               else
                  Initial_Mission_Viability_Logic;

                  Initial_Energy_Test_Done := True;
                  Energy_Test_Succeded :=
                    Initial_Energy_Compatible_With_Mission;
               end if;

            elsif On_State = RUNNING
              and then Running_State = FLIGHT
              and then Current_Flight_Phase = DESCENT
            then
               Mission_Termination_Control;
            end if;
         end if;
      end if;

      Update_States;
      Write_Outputs;
   end Run;

end MMS.F_PT.F_MM.Behavior.Guarantees;
