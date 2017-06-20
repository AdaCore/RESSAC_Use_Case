--  This package provides the behavioural specification of F_MM. It is
--  expressed as a contract on a Run procedure which reprents the modifications
--  performed on the state State of F_MM at each cycle of the functionality.
--  We write the contract using a Contract_Cases and we use SPARK to ensure
--  that:
--   - Information about the current State of the module are only accessed
--     when it makes sense (represented as preconditions over accessors).
--   - A single behaviour is specified for each case in the specification.
--   - There is a behaviour is specified for every case in the specification.

package MMS.F_PT.F_MM.Behavior with
  SPARK_Mode,
  Abstract_State => State
is
   pragma Unevaluated_Use_Of_Old (Allow);

   -----------------------------------------
   -- States of the automaton in Figure 3 --
   -----------------------------------------

   type Power_State_Type is (ON, OFF);

   function Power_State return Power_State_Type with
     Global => State;

   type On_State_Type is (INIT, RUNNING, CANCELLED, COMPLETE, ABORTED);

   function On_State return On_State_Type with
     Global => State,
     Pre    => Power_State = ON;

   type Running_State_Type is (TAKE_OFF, CLIMB, CRUISE, DESCENT, LANDING);

   function Running_State return Running_State_Type with
     Global => State,
     Pre => Power_State = ON
     and then On_State = RUNNING;

   function Navigation_Mode return Navigation_Mode_Type with
     Global => State,
     Pre => Power_State = ON
     and then On_State in INIT | RUNNING;

   function Operating_Mode return Navigation_Option_Type with
     Global => State,
     Pre => Power_State = ON
     and then On_State = RUNNING
     and then Navigation_Mode = RP;

   -----------------------------------------
   -- Guards of the automaton in Figure 3 --
   -----------------------------------------

   function Boarding_Completed return Boolean with
     Global => State,
     Pre  => Power_State = ON
     and then On_State = INIT,
     Post =>
       (if Boarding_Completed'Result then
          Payload_Bay_Closed
        and then Mission_Parameters_Defined
        and then Energy_Compatible_With_Mission);

   function Power_On return Boolean with
     Global => State;

   function Power_Off return Boolean with
     Global => State,
     Post => Power_Off'Result = not Power_On;

   function Mission_Abort_Received return Boolean with
     Global => State,
     Pre => Power_State = ON;

   function Start_Or_Go_Received return Boolean with
     Global => State,
     Pre => Power_State = ON
     and then On_State = INIT;

   function Take_Off_Over return Boolean with
     Global => State,
     Pre => Power_State = ON
     and then On_State = RUNNING
     and then Running_State = TAKE_OFF;

   function Descent_Over return Boolean with
     Global => State,
     Pre => Power_State = ON
     and then On_State = RUNNING
     and then Running_State = DESCENT;

   function Landed return Boolean with
     Global => State,
     Pre => Power_State = ON
     and then On_State = RUNNING
     and then Running_State = LANDING;

   function Operating_Mode_Changed return Boolean with
     Global => State,
     Pre => Power_State = ON
     and then On_State = RUNNING
     and then (Running_State in CLIMB | CRUISE | DESCENT)
     and then Navigation_Mode = RP;

   function Cruise_Altitude_Reached return Boolean with
     Global => State,
     Pre => Power_State = ON
     and then On_State = RUNNING
     and then (Running_State in CLIMB | DESCENT);

   ----------------
   -- Properties --
   ----------------

   function Energy_Compatible_With_Mission return Boolean with
     Global => State,
     Pre => Power_State = ON
     and then On_State in INIT | RUNNING
     and then (if On_State = RUNNING then Running_State = CRUISE);

   function Mission_Parameters_Defined return Boolean with
     Global => State,
     Pre => Power_State = ON
     and then On_State = INIT;

   function Payload_Bay_Closed return Boolean with
     Global => State,
     Pre => Power_State = ON
     and then On_State = INIT;

   function Emergency_Landing return Boolean with
     Global => State,
     Pre => Power_State = ON
     and then On_State = CANCELLED;

   function Mission_Cancellation_Signaled return Boolean with
     Global => State;

   function Mission_Range return Current_Range_Type with
     Global => State;

   function Operating_Point return Operating_Point_Type with
     Global => State;

   ---------------------------------------
   -- Behavioural Specification of F_MM --
   ---------------------------------------

   procedure Run with
     Global         => (In_Out => State),
     Post           =>

       --  RP mode enables modification of range parameter before take-off.

       (if not (Power_State'Old = ON
                and then On_State'Old = INIT
                and then Navigation_Mode'Old = RP)
        then Mission_Range = Mission_Range'Old)

       --  RP mode enables modification of altitude and speed parameters at any
       --  time.

     and then
       (if Navigation_Mode'Old = A
        then Operating_Point = Operating_Point'Old),
     Contract_Cases =>
       (Power_State = OFF
        and then Power_Off
        =>
          Power_State = OFF,

        Power_State = OFF
        and then Power_On
        =>
          Power_State = ON
        and then On_State = INIT,

        Power_State = ON
        and then Power_Off
        =>
          Power_State = OFF,

        Power_State = ON
        and then Power_On
        and then (On_State in INIT | RUNNING)
        and then Mission_Abort_Received
        =>
          Power_State = ON
        and then On_State = ABORTED,

        Power_State = ON
        and then Power_On
        and then On_State = INIT
        and then not Mission_Abort_Received
        and then not Boarding_Completed
        =>
          Power_State = ON
        and then On_State = INIT,

        Power_State = ON
        and then Power_On
        and then On_State = INIT
        and then not Mission_Abort_Received
        and then Boarding_Completed
        and then not Start_Or_Go_Received
        =>
          Power_State = ON
        and then On_State = INIT,

        Power_State = ON
        and then Power_On
        and then On_State = INIT
        and then not Mission_Abort_Received
        and then Boarding_Completed
        and then Start_Or_Go_Received
        =>
          Power_State = ON
        and then On_State = RUNNING
        and then Running_State = TAKE_OFF,

        Power_State = ON
        and then On_State = RUNNING
        and then Running_State = TAKE_OFF
        and then Power_On
        and then not Mission_Abort_Received
        and then Take_Off_Over
        =>
          Power_State = ON
        and then On_State = RUNNING
        and then Running_State = CLIMB,

        Power_State = ON
        and then On_State = RUNNING
        and then Running_State = TAKE_OFF
        and then Power_On
        and then not Mission_Abort_Received
        and then not Take_Off_Over
        =>
          Power_State = ON
        and then On_State = RUNNING
        and then Running_State = TAKE_OFF,

        Power_State = ON
        and then On_State = RUNNING
        and then Running_State = CRUISE
        and then Power_On
        and then not Mission_Abort_Received
        and then not Energy_Compatible_With_Mission
        =>
          Power_State = ON
        and then On_State = CANCELLED
        and then Mission_Cancellation_Signaled
        and then Emergency_Landing,

        Power_State = ON
        and then On_State = RUNNING
        and then Running_State = DESCENT
        and then Power_On
        and then not Mission_Abort_Received
        and then Descent_Over
        =>
          Power_State = ON
        and then On_State = RUNNING
        and then Running_State = LANDING,

        Power_State = ON
        and then On_State = RUNNING
        and then (Running_State in CLIMB | CRUISE | DESCENT)
        and then Power_On
        and then not Mission_Abort_Received
        and then Navigation_Mode = RP
        and then
          (if Running_State = CRUISE then Energy_Compatible_With_Mission)
        and then
          (if Running_State = DESCENT then not Descent_Over)
        and then Operating_Mode_Changed
        =>
          Power_State = ON
        and then On_State = RUNNING
        and then (Running_State in CLIMB | DESCENT),

        Power_State = ON
        and then On_State = RUNNING
        and then (Running_State in CLIMB | DESCENT)
        and then Power_On
        and then not Mission_Abort_Received
        and then (if Navigation_Mode = RP then not Operating_Mode_Changed)
        and then (if Running_State = DESCENT then not Descent_Over)
        and then Cruise_Altitude_Reached
        =>
          Power_State = ON
        and then On_State = RUNNING
        and then Running_State = CRUISE,

        Power_State = ON
        and then On_State = RUNNING
        and then (Running_State in CLIMB | DESCENT | CRUISE)
        and then Power_On
        and then not Mission_Abort_Received
        and then
          (if Running_State = CRUISE then Energy_Compatible_With_Mission)
        and then (if Navigation_Mode = RP then not Operating_Mode_Changed)
        and then
          (if Running_State in CLIMB | DESCENT then
                 not Cruise_Altitude_Reached)
        and then (if Running_State = DESCENT then not Descent_Over)
        =>
          Power_State = ON
        and then On_State = RUNNING
        and then Running_State = Running_State'Old,

        Power_State = ON
        and then On_State = RUNNING
        and then Running_State = LANDING
        and then Power_On
        and then not Mission_Abort_Received
        and then Landed
        =>
          Power_State = ON
        and then On_State = COMPLETE,

        Power_State = ON
        and then On_State = RUNNING
        and then Running_State = LANDING
        and then Power_On
        and then not Mission_Abort_Received
        and then not Landed
        =>
          Power_State = ON
        and then On_State = RUNNING
        and then Running_State = LANDING,

        Power_State = ON
        and then Power_On
        and then (On_State in CANCELLED .. ABORTED)
        =>
          Power_State = ON
        and then On_State = On_State'Old);

end MMS.F_PT.F_MM.Behavior;
