--  This package provides the behavioural specification of F_MM. It is
--  expressed as a contract on a Run procedure which reprents the modifications
--  performed on the state State of F_MM at each cycle of the functionality.
--  We write the contract using a Contract_Cases and we use SPARK to ensure
--  that:
--   - Information about the current State of the module are only accessed
--     when it makes sense (represented as preconditions over accessors).
--   - A single behaviour is specified for each case in the specification.
--   - There is a behaviour is specified for every case in the specification.

with Types; use Types;
with External;

package MMS.F_PT.F_MM.Behavior with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);

   ------------
   -- Inputs --
   ------------

   function Navigation_Mode_From_CP return Navigation_Mode_Type with
     Global => Private_State;

   function Navigation_Mode_From_GS return Navigation_Mode_Type with
     Global => Private_State;

   function Operating_Point_From_GS_Received return Boolean with
     Global => Private_State;

   function Operating_Point_From_GS return Operating_Point_Type with
     Global => Private_State;

   function USB_Key_Present return Boolean with
     Global => Private_State;

   function Operating_Point_From_USB_Key return Operating_Point_Type with
     Global => Private_State;

   function Current_Range return Current_Range_Type with
     Global => Private_State;

   function Current_Speed return Current_Speed_Type with
     Global => Private_State;

   function Current_Altitude return Current_Altitude_Type with
     Global => Private_State;

   function Current_Flight_Phase return Flight_Phase_Type with
     Global => Private_State,
     Pre    => Power_State = ON
     and then On_State = RUNNING
     and then Running_State = FLIGHT;

   -----------------------------------------
   -- States of the automaton in Figure 3 --
   -----------------------------------------

   type Power_State_Type is (ON, OFF);

   function Power_State return Power_State_Type with
     Global => Private_State;

   type On_State_Type is (INIT, RUNNING, CANCELLED, COMPLETE, ABORTED);

   function On_State return On_State_Type with
     Global => Private_State,
     Pre    => Power_State = ON;

   type Running_State_Type is (TAKE_OFF, FLIGHT, LANDING);

   function Running_State return Running_State_Type with
     Global => Private_State,
     Pre => Power_State = ON
     and then On_State = RUNNING;

   function Navigation_Mode return Navigation_Mode_Type
   is (if Navigation_Mode_From_CP = A then A
       else Navigation_Mode_From_GS)
   with
     Global => Private_State,
     Pre => Power_State = ON
     and then On_State in INIT | RUNNING;

   function Operating_Mode return Navigation_Option_Type with
     Global => Private_State,
     Pre => Power_State = ON
     and then On_State = RUNNING
     and then Navigation_Mode = RP;

   -----------------------------------------
   -- Guards of the automaton in Figure 3 --
   -----------------------------------------

   function Power_On return Boolean with
     Global => Private_State;

   function Power_Off return Boolean with
     Global => Private_State,
     Post => Power_Off'Result = not Power_On;

   function Mission_Abort_Received return Boolean with
     Global => Private_State,
     Pre => Power_State = ON;

   function Start_Or_Go_Received return Boolean with
     Global => Private_State,
     Pre => Power_State = ON
     and then On_State = INIT;

   function Take_Off_Over return Boolean with
     Global => Private_State,
     Pre => Power_State = ON
     and then On_State = RUNNING
     and then Running_State = TAKE_OFF;

   function Descent_Over return Boolean with
     Global => Private_State,
     Pre => Power_State = ON
     and then On_State = RUNNING
     and then Running_State = FLIGHT
     and then Current_Flight_Phase = DESCENT;

   function Landed return Boolean is
     (Current_Speed = 0 and Current_Altitude = 0)
   with
     Global => Private_State,
     Pre => Power_State = ON
     and then On_State = RUNNING
     and then Running_State = LANDING;

   ----------------
   -- Properties --
   ----------------

   function Energy_Compatible_With_Mission return Boolean with
     Global => Private_State,
     Pre => Power_State = ON
     and then On_State in INIT | RUNNING
     and then (if On_State = RUNNING then
                 Running_State = FLIGHT and then Current_Flight_Phase = CRUISE);

   function Mission_Parameters_Defined return Boolean is
     (USB_Key_Present
      or else (Navigation_Mode_From_CP = RP
               and then Operating_Point_From_GS_Received))
   with
     Global => Private_State,
     Pre => Power_State = ON
     and then On_State = INIT;

   function Payload_Bay_Closed return Boolean with
     Global => Private_State,
     Pre => Power_State = ON
     and then On_State = INIT;

   function Mission_Cancellation_Signaled return Boolean with
     Global => Private_State;

   -------------
   -- Outputs --
   -------------

   function Ready_For_Takeoff return Boolean is
     (Payload_Bay_Closed
      and then Mission_Parameters_Defined
      and then Energy_Compatible_With_Mission)
   with
   Global => Private_State,
   Pre  => Power_State = ON
     and then On_State = INIT;

   function Emergency_Landing return Boolean with
     Global => Private_State,
     Pre => Power_State = ON
     and then On_State = CANCELLED;

   function Mission_Range return Current_Range_Type with
     Global => Private_State;

   function Operating_Point return Operating_Point_Type with
     Global => Private_State;

   ---------------------------------------
   -- Behavioural Specification of F_MM --
   ---------------------------------------

   procedure Read_Inputs with
   --  Read values of inputs once and for all and update the current state
     Global => (In_Out => Private_State,
                Input  => External.State);

   procedure Write_Outputs with
   --  Compute values of outputs from the current state
     Global => (Input  => Private_State,
                Output => Output_State);

   procedure Run with
   -- Do:
   --  - Compute the new state of the automaton

     Global         => (In_Out => Private_State),
     Post           =>

       --  RP mode enables modification of range parameter before take-off.

       (if not (Power_State'Old = ON
                and then On_State'Old = INIT
                and then Navigation_Mode'Old = RP)
        then Mission_Range = Mission_Range'Old)

       --  RP mode enables modification of altitude and speed parameters at any
       --  time (but not at landing, it is frozen...).

     and then
       (if (Power_State'Old = ON
            and then On_State'Old in INIT | RUNNING
            and then Navigation_Mode'Old = A)
        then Operating_Point = Operating_Point'Old)

       --  The operating point is frozen once landing is activated.

     and then
       (if Power_State'Old = ON
        and then On_State'Old = RUNNING
        and then Running_State = LANDING
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
        and then not Ready_For_Takeoff
        =>
          Power_State = ON
        and then On_State = INIT,

        Power_State = ON
        and then Power_On
        and then On_State = INIT
        and then not Mission_Abort_Received
        and then Ready_For_Takeoff
        and then not Start_Or_Go_Received
        =>
          Power_State = ON
        and then On_State = INIT,

        Power_State = ON
        and then Power_On
        and then On_State = INIT
        and then not Mission_Abort_Received
        and then Ready_For_Takeoff
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
        and then Running_State = FLIGHT,

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
        and then Running_State = FLIGHT
        and then Current_Flight_Phase = CRUISE
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
        and then Running_State = FLIGHT
        and then Current_Flight_Phase = DESCENT
        and then Power_On
        and then not Mission_Abort_Received
        and then Descent_Over
        =>
          Power_State = ON
        and then On_State = RUNNING
        and then Running_State = LANDING,

        Power_State = ON
        and then On_State = RUNNING
        and then Running_State = FLIGHT
        and then Power_On
        and then not Mission_Abort_Received
        and then
          (if Current_Flight_Phase = CRUISE then Energy_Compatible_With_Mission)
        and then
          (if Current_Flight_Phase = DESCENT then not Descent_Over)
        =>
          Power_State = ON
        and then On_State = RUNNING
        and then Running_State = FLIGHT,

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

private

   procedure Operating_Point_Update_Management with
   --  Compute the value of Operating_Point

     Global => (In_Out => Private_State),
     Contract_Cases =>
       (Navigation_Mode_From_CP = A
        or else not Operating_Point_From_GS_Received
        =>
          Operating_Point = Operating_Point_From_USB_Key,

        Navigation_Mode_From_CP = RP
        and then Operating_Point_From_GS_Received
        =>
          (if Power_State = ON
           and then On_State = RUNNING
           and then Running_State = LANDING
           then Operating_Point = Operating_Point'Old
           else Operating_Point = Operating_Point_From_GS));

end MMS.F_PT.F_MM.Behavior;
