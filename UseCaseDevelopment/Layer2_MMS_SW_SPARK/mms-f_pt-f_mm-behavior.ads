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
with MMS.F_PT.F_MM.Data;
private with MMS.F_PT.F_MM.State;
with MMS.F_PT.Data;

package MMS.F_PT.F_MM.Behavior with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);

   ------------
   -- Inputs --
   ------------

   function Power_On return Boolean;

   function Payload_Bay_Closed return Boolean with
     Pre => Power_State = ON
     and then On_State = INIT;

   function Payload_Mass_Given return Boolean with
     Pre => Power_State = ON;
   --  ???  Should we assume that Payload_Mass is always given after takeoff?
   --  same question for usb key

   function Payload_Mass return Payload_Mass_Type with
     Pre => Power_State = ON;

   function Navigation_Mode_From_CP return Navigation_Mode_Type;

   function Navigation_Mode_From_GS_Received return Boolean;

   function Navigation_Mode_From_GS return Navigation_Mode_Type with
     Pre => Navigation_Mode_From_GS_Received;

   function Operating_Mode_From_CP return Navigation_Option_Type;

   function Operating_Mode_From_GS_Received return Boolean;

   function Operating_Mode_From_GS return Navigation_Option_Type with
     Pre => Operating_Mode_From_GS_Received;

   function Navigation_Parameters_From_GS_Received return Boolean;

   function Navigation_Parameters_From_GS return Navigation_Parameters_Type with
     Pre => Navigation_Parameters_From_GS_Received;

   function USB_Key_Present return Boolean;

   function Navigation_Parameters_From_USB_Key return Navigation_Parameters_Type
   with
     Pre => USB_Key_Present;

   function Mission_Abort_Received return Boolean with
     Pre => Power_State = ON;

   function Start_Or_Go_Received return Boolean with
     Pre => Power_State = ON
     and then On_State = INIT;

   function Current_Range return Current_Range_Type;

   function Current_Speed return Current_Speed_Type;

   function Current_Altitude return Current_Altitude_Type;

   function Current_Flight_Phase return Flight_Phase_Type with
     Pre => Power_State = ON
     and then On_State = RUNNING
     and then Running_State = FLIGHT;

   function Energy_Level return Energy_Level_Type with
     Pre => Power_State = ON;

   function Mission_Parameters_Defined return Boolean is
     (USB_Key_Present
      or else (Navigation_Mode_From_CP = RP
               and then Navigation_Parameters_From_GS_Received));

   function Init_Completed return Boolean is
     (Payload_Bay_Closed
      and then Payload_Mass_Given
      and then Mission_Parameters_Defined)
   with
   Pre    => Power_State = ON
     and then On_State = INIT;

   -----------------------------------------
   -- States of the automaton in Figure 3 --
   -----------------------------------------

   type Power_State_Type is (ON, OFF);

   function Power_State return Power_State_Type with
     Global => Private_State;

   type On_State_Type is (INIT, RUNNING, COMPLETE, ABORTED);

   function On_State return On_State_Type with
     Global => Private_State,
     Pre    => Power_State = ON;

   type Running_State_Type is (TAKE_OFF, FLIGHT, LANDING);

   function Running_State return Running_State_Type with
     Global => Private_State,
     Pre => Power_State = ON
     and then On_State = RUNNING;

   type Init_State_Type is (PREPARATION, READY, CANCELLED);

   function Init_State return Init_State_Type with
     Global => Private_State,
     Pre => Power_State = ON
     and then On_State = INIT;

   -----------------------------
   -- Properties and Entities --
   -----------------------------

   function Power_Off return Boolean is (not Power_On);

   function Take_Off_Over return Boolean with
     Global => Private_State,
     Pre => Power_State = ON
     and then On_State = RUNNING
     and then Running_State = TAKE_OFF;

   function Descent_Over return Boolean with
     Global => Private_State,
     Pre => Power_State = ON
     and then On_State = RUNNING
     and then Running_State = FLIGHT;

   function Landed return Boolean is
     (Current_Speed = 0 and Current_Altitude = 0)
   with
     Pre => Power_State = ON
     and then On_State = RUNNING
     and then Running_State = LANDING;

   function Mission_Range_From_Navigation_Parameters
     return Current_Range_Type
   with Pre => Mission_Parameters_Defined;
   --  Fetch distance from State.Navigation_Parameters and do the appropriate
   --  conversion.

   function Operating_Point_From_Navigation_Parameters
     return Operating_Point_Type
   with Pre => Mission_Parameters_Defined;
   --  Fetch altitude and speed from State.Navigation_Parameters and do the
   --  appropriate conversions.

   function Navigation_Mode return Navigation_Mode_Type with
     Global => Private_State,
     Pre    => Power_State = ON
     and then On_State in INIT | RUNNING;

   function Operating_Mode return Navigation_Option_Type with
     Global => Private_State,
     Pre    => Power_State = ON
     and then On_State = RUNNING
     and then Navigation_Mode = RP;

   function Initial_Energy_Compatible_With_Mission return Boolean with
     Global => Private_State;

   function In_Flight_Energy_Compatible_With_Mission return Boolean with
     Global => Private_State;

   function Emergency_Landing return Boolean is
     (On_State = ABORTED)
   with
     Global => Private_State,
     Pre    => Power_State = ON;

   function Mission_Range return Current_Range_Type with
     Global => (Input => Private_State, Proof_In => Input_State),
     Pre => Mission_Parameters_Defined;

   function Operating_Point return Operating_Point_Type with
     Global => (Input => Private_State, Proof_In => Input_State),
     Pre => Mission_Parameters_Defined;

   function Mission_Aborted_Signaled return Boolean with
     Global => Private_State,
     Pre    => Power_State = ON;

   function Mission_Cancelled_Signaled return Boolean with
     Global => Private_State,
     Pre    => Power_State = ON
     and then On_State = INIT;

   function Aborted_For_Energy_Reasons return Boolean with
     Global => Private_State,
     Pre    => Power_State = ON
     and then On_State = ABORTED;

   ---------------------------------------
   -- Behavioural Specification of F_MM --
   ---------------------------------------

   procedure Read_Inputs with
   --  Read values of inputs once and for all and update the current state
     Global => (Output => Input_State,
                Input  => External.State);

   procedure Write_Outputs with
   --  Compute values of outputs from the current state
     Global => (Input  => Private_State,
                Output => Output_State);

   procedure Run with
   -- Do:
   --  - Compute the new state of the automaton

     Global         => (In_Out => Private_State, Input => Input_State),
     Post           =>

       --  RP mode enables modification of range parameter before take-off.

       (if not (Power_State'Old = ON
                and then On_State'Old = INIT
                and then Navigation_Mode'Old = RP)
        then Mission_Range = Mission_Range'Old
        elsif Mission_Parameters_Defined
        then Mission_Range = Mission_Range_From_Navigation_Parameters)

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
        and then On_State = INIT
        and then Init_State = PREPARATION,

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
        and then On_State = ABORTED
        and then Aborted_For_Energy_Reasons = False
        and then Mission_Aborted_Signaled,

        Power_State = ON
        and then Power_On
        and then On_State = INIT
        and then not Mission_Abort_Received
        and then not Init_Completed
        =>
          Power_State = ON
        and then On_State = INIT
        and then Init_State = PREPARATION,

        Power_State = ON
        and then Power_On
        and then On_State = INIT
        and then not Mission_Abort_Received
        and then Init_Completed
        and then not Start_Or_Go_Received
        =>
          Power_State = ON
        and then On_State = INIT
        and then
          (if Initial_Energy_Compatible_With_Mission then
             Init_State = READY
           else
             Init_State = CANCELLED
             and then Mission_Cancelled_Signaled),

        Power_State = ON
        and then Power_On
        and then On_State = INIT
        and then not Mission_Abort_Received
        and then Init_Completed
        and then Start_Or_Go_Received
        =>
          (if Initial_Energy_Compatible_With_Mission then
                 Power_State = ON
             and then On_State = RUNNING
             and then Running_State = TAKE_OFF
           else
             Power_State = ON
             and then On_State = INIT
             and then Init_State = CANCELLED
             and then Mission_Cancelled_Signaled),

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
        and then Power_On
        and then not Mission_Abort_Received
        =>
          (if Current_Flight_Phase = CRUISE
             and then not In_Flight_Energy_Compatible_With_Mission
           then
               Power_State = ON
             and then On_State = ABORTED
             and then Aborted_For_Energy_Reasons = True
             and then Mission_Aborted_Signaled
             and then Emergency_Landing
           elsif Current_Flight_Phase = DESCENT
             and then Descent_Over
           then
               Power_State = ON
             and then On_State = RUNNING
             and then Running_State = LANDING
           else
               Power_State = ON
             and then On_State = RUNNING
             and then Running_State = FLIGHT),

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
        and then (On_State in COMPLETE .. ABORTED)
        =>
          Power_State = ON
        and then On_State = On_State'Old);

private

   ----------------------------
   --  Definitions of Inputs --
   ----------------------------

   function Power_On return Boolean is
     (State.Input_On_OFF_Push_Button);

   function Payload_Bay_Closed return Boolean is
     (State.Input_Bay_Switch = CLOSED);

   function Payload_Mass_Given return Boolean is
     (State.Input_Payload_Mass /= 99);

   function Payload_Mass return Payload_Mass_Type is
     (State.Input_Payload_Mass);

   function Navigation_Mode_From_CP return Navigation_Mode_Type is
     (State.Input_Mode_Switch);

   function Navigation_Mode_From_GS_Received return Boolean is
     (State.Input_Navigation_Mode.Present);

   function Navigation_Mode_From_GS return Navigation_Mode_Type is
     (State.Input_Navigation_Mode.Content);

   function Operating_Mode_From_CP return Navigation_Option_Type is
     (ALTITUDE); --  ??? what is the default operating mode in A mode?

   function Operating_Mode_From_GS_Received return Boolean is
     (State.Input_Navigation_Option.Present);

   function Operating_Mode_From_GS return Navigation_Option_Type is
     (State.Input_Navigation_Option.Content);

   function Navigation_Parameters_From_GS_Received return Boolean is
     (State.Input_Navigation_Parameters.Present);

   function Navigation_Parameters_From_GS return Navigation_Parameters_Type is
     (State.Input_Navigation_Parameters.Content);

   function USB_Key_Present return Boolean is
     (State.Input_USB_Key.Present);

   function Navigation_Parameters_From_USB_Key return Navigation_Parameters_Type is
     (State.Input_USB_Key.Content);

   function Mission_Abort_Received return Boolean is
     (State.Input_Mission_Abort);

   function Start_Or_Go_Received return Boolean is
     (State.Input_Go or State.Input_Start_Push_Button);

   function Current_Range return Current_Range_Type is
     (State.Input_Current_Range);

   function Current_Speed return Current_Speed_Type is
     (State.Input_Current_Speed);

   function Current_Altitude return Current_Altitude_Type is
     (State.Input_Current_Altitude);

   function Current_Flight_Phase return Flight_Phase_Type is
     (State.Input_Current_Flight_Phase);

   function Energy_Level return Energy_Level_Type is
     (State.Input_Energy_Level);

   -------------------
   -- Tasks of F_MM --
   -------------------

   function Navigation_Parameters return Navigation_Parameters_Type is
     (State.Navigation_Parameters)
   with Pre => Mission_Parameters_Defined;

   procedure Management_Of_Navigation_Mode with
   --  Compute the value of Navigation_Mode / Options / Parameters (see 6.9.4)

     Post   => Navigation_Mode =

     --  In case of conflict on the navigation mode, CP prevails over GS.

     (if Navigation_Mode_From_CP = A
      or else not Navigation_Mode_From_GS_Received
      then Navigation_Mode_From_CP

     --  If CP states the mode to RC then GS can choose the navigation mode.

      else Navigation_Mode_From_GS)

     and then Operating_Mode =
       (if Navigation_Mode = A
        or else not Operating_Mode_From_GS_Received
        then Operating_Mode_From_CP
        else Operating_Mode_From_GS)

     and then
         (if Mission_Parameters_Defined then
            Navigation_Parameters =
              (if Navigation_Mode = A
               or else not Navigation_Parameters_From_GS_Received
               then Navigation_Parameters_From_USB_Key
               else Navigation_Parameters_From_GS));

   procedure Operating_Point_Update_Management with
   --  Compute the value of Operating_Point

     Pre    =>
       Mission_Parameters_Defined
       and then Power_State = ON
       and then On_State in INIT | RUNNING,

     --  F_MM ensures freeze of the operating point once landing is activated.

     Post   =>
         (if Power_State = ON
           and then On_State = RUNNING
           and then Running_State = LANDING
          then Operating_Point = Operating_Point'Old
          else Operating_Point = Operating_Point_From_Navigation_Parameters);

   ------------------------------
   --  Mission_Viability_Logic --
   ------------------------------

   function Mission_Profile return Mission_Profile_Type with
   --  Assemble the mission profile

     Pre  => Power_State = ON,
     Post => Mission_Profile'Result =
        (Mass     => Payload_Mass,
         Distance => Current_Range,
         Altitude => Current_Altitude,
         Speed    => Current_Speed);

   function Appropriate_Tabulating_Function return Viability_Domain_Mesh_Type
   --  Select the tabulated function corresponding to the navigation mode

   with
     Pre  => Power_State = ON
     and then On_State in INIT | RUNNING,
     Post => Appropriate_Tabulating_Function'Result =
         (if On_State = INIT and then Navigation_Mode = A
          then Data.Amode_Initial_Domain_Mesh
          elsif On_State = INIT and then Navigation_Mode = RP
          then Data.RPmode_Initial_Domain_Mesh
          elsif Navigation_Mode = A
          then Data.Amode_Cruise_Domain_Mesh
          else Data.RPmode_Cruise_Domain_Mesh);

   function Distance_With_Neighbour
     (Neighbour : Mission_Profile_Type) return Mission_Profile_Distance_Type
   with
     Pre => Power_State = ON
       and then On_State in INIT | RUNNING;
   --  Compute the distance between Mission_Profile and its Neighbour.

   function Nearest_Neighbours return Neighbour_Mission_Profile_Array_Type with
   --  Compute the nearest neighbous of Mission_Profile in
   --  Appropriate_Tabulating_Function, and the distance of Mission_Profile to
   --  its nearest neignbours.

     Pre  => Power_State = ON
     and then On_State in INIT | RUNNING,
     Post =>
       (for all Neighbour_Center of Nearest_Neighbours'Result =>
          Neighbour_Center.Mission_Profile.M in
            MMS.F_PT.Data.Payload_Mass_Grid'Range
        and then Neighbour_Center.Mission_Profile.D in
          Appropriate_Tabulating_Function'Range (1)
        and then Neighbour_Center.Mission_Profile.A in
          Appropriate_Tabulating_Function'Range (2)
        and then Neighbour_Center.Mission_Profile.S in
          Appropriate_Tabulating_Function'Range (3)
        and then Neighbour_Center.Distance =
          Distance_With_Neighbour
            (Mission_Profile_Type'
               (Mass     =>
                   MMS.F_PT.Data.Payload_Mass_Grid
                     (Neighbour_Center.Mission_Profile.M),
                Distance =>
                  Appropriate_Tabulating_Function
                    (Neighbour_Center.Mission_Profile.D,
                     Neighbour_Center.Mission_Profile.A,
                     Neighbour_Center.Mission_Profile.S).Distance,
                Altitude =>
                  Appropriate_Tabulating_Function
                    (Neighbour_Center.Mission_Profile.D,
                     Neighbour_Center.Mission_Profile.A,
                     Neighbour_Center.Mission_Profile.S).Altitude,
                Speed    =>
                  Appropriate_Tabulating_Function
                    (Neighbour_Center.Mission_Profile.D,
                     Neighbour_Center.Mission_Profile.A,
                     Neighbour_Center.Mission_Profile.S).Speed)));

   function Extract_Energy_Level_For_Neighbour
     (Neighbour : Center_Mission_Profile_Type) return Energy_Level_Type
   --  Extract energy level for the neighbour.

   with
     Pre  => Power_State = ON
     and then On_State in INIT | RUNNING,
     Post => Extract_Energy_Level_For_Neighbour'Result =
         (if On_State = INIT and then Navigation_Mode = A
          then Data.Viability_Amode_Initial (M => Neighbour.M,
                                             D => Neighbour.D,
                                             A => Neighbour.A,
                                             S => Neighbour.S)
          elsif On_State = INIT and then Navigation_Mode = RP
          then Data.Viability_RPmode_Initial (M => Neighbour.M,
                                              D => Neighbour.D,
                                              A => Neighbour.A,
                                              S => Neighbour.S)
          elsif Navigation_Mode = A
          then Data.Viability_Amode_Cruise (M => Neighbour.M,
                                            D => Neighbour.D,
                                            A => Neighbour.A,
                                            S => Neighbour.S)
          else Data.Viability_RPmode_Cruise (M => Neighbour.M,
                                             D => Neighbour.D,
                                             A => Neighbour.A,
                                             S => Neighbour.S));

   function Interpolated_Energy_Level return Energy_Level_Type;
   --  Compute the interpolation of the energy levels of the neighbours of
   --  Mission_Profile by distance-based averaging.

   procedure Initial_Mission_Viability_Logic with
   --  Compute the value of Initial_Energy_Compatible_With_Mission. It should
   --  be computed when Init_Completed is True.

     Pre  => Power_State = ON
     and then On_State = INIT
     and then Init_Completed,
     Post => Initial_Energy_Compatible_With_Mission =

   --  In A mode, use a 30% energy margin.

       ((if Navigation_Mode = A then Interpolated_Energy_Level * 13 / 10

   --  In RP mode, use a 10% energy margin.

         else Interpolated_Energy_Level * 11 / 10) >= Energy_Level);

   procedure In_Flight_Mission_Viability_Logic with
   --  Compute the value of In_Flight_Energy_Compatible_With_Mission. It should
   --  be repeated at a periodic rate of F_Viability.
   --  Set In_Flight_Energy_Compatible_With_Mission to True if Energy_Level is
   --  at least the Interpolated_Energy_Level plus an enery margin. When
   --  EstimatedTotalMass increases, and even more so if it increases quickly,
   --  F_MM applies greater safety margins (see #17).

     Pre => Power_State = ON
     and then On_State = RUNNING
     and then Running_State = FLIGHT
     and then Current_Flight_Phase = CRUISE;

end MMS.F_PT.F_MM.Behavior;
