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
private with MMS.F_PT.F_MM.Data;
private with MMS.F_PT.F_MM.State;
with MMS.F_PT.Data;

package MMS.F_PT.F_MM.Behavior with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);

   ------------
   -- Inputs --
   ------------

   function Power_On return Boolean;

   function Payload_Bay_Closed return Boolean with
     Pre => Power_On;

   function Payload_Mass_Given return Boolean with
     Pre => Power_On;

   function Payload_Mass return Payload_Mass_Type with
     Pre => Power_On and then Payload_Mass_Given;

   function Navigation_Mode_From_CP return Navigation_Mode_Type with
     Pre => Power_On;

   function Navigation_Mode_From_GS_Received return Boolean with
     Pre => Power_On;

   function Navigation_Mode_From_GS return Navigation_Mode_Type with
     Pre => Power_On and then Navigation_Mode_From_GS_Received;

   function Operating_Mode_From_USB_Key return Navigation_Option_Type with
     Pre => Power_On and then USB_Key_Present;

   function Operating_Mode_From_GS_Received return Boolean with
     Pre => Power_On;

   function Operating_Mode_From_GS return Navigation_Option_Type with
     Pre => Power_On and then Operating_Mode_From_GS_Received;

   function Navigation_Parameters_From_GS_Received return Boolean with
     Pre => Power_On;

   function Navigation_Parameters_From_GS return Navigation_Parameters_Type with
     Pre => Power_On and then Navigation_Parameters_From_GS_Received;

   function USB_Key_Present return Boolean with
     Pre => Power_On;

   function Navigation_Parameters_From_USB_Key return Navigation_Parameters_Type
   with
     Pre => Power_On and then USB_Key_Present;

   function Mission_Abort_Received return Boolean with
     Pre => Power_On;

   function Start_Or_Go_Received return Boolean with
     Pre => Power_On;

   function Current_Range return Current_Range_Type with
   Pre => Power_On;

   function Current_Speed return Current_Speed_Type with
   Pre => Power_On;

   function Current_Altitude return Current_Altitude_Type with
   Pre => Power_On;

   function Current_Flight_Phase return Flight_Phase_Type with
     Pre => Power_On;

   function Energy_Level return Energy_Level_Type with
     Pre => Power_On;

   function Mission_Parameters_Defined return Boolean is
     (USB_Key_Present
      or else (Navigation_Mode_From_CP = RP
               and then Navigation_Parameters_From_GS_Received
               and then Operating_Mode_From_GS_Received))
   with
   Pre  => Power_On;

   function Init_Completed return Boolean is
     (Payload_Bay_Closed
      and then Payload_Mass_Given
      and then Mission_Parameters_Defined)
   with
   Pre => Power_On;

   -----------------------------------------
   -- States of the automaton in Figure 3 --
   -----------------------------------------

   function Power_State return Power_State_Type;

   function On_State return On_State_Type with
     Pre    => Power_State = ON;

   function Running_State return Running_State_Type with
     Pre => Power_State = ON
     and then On_State = RUNNING;

   function Init_State return Init_State_Type with
     Pre => Power_State = ON
     and then On_State = INIT;

   function Aborted_For_Energy_Reasons return Boolean with
     Pre    => Power_State = ON
     and then On_State = ABORTED;

   function State_Invariant return Boolean is
     (Power_On = (Power_State = On)
      and then (if Power_State = On and then On_State = RUNNING then
                     Init_Completed));
   --  Global assumptions, should be maintained by the task main loop.

   -----------------------------
   -- Properties and Entities --
   -----------------------------

   function Navigation_Mode return Navigation_Mode_Type with
     Pre    => Power_On;

   function Operating_Mode_From_Parameters return Navigation_Option_Type with
     Pre    => Power_On
     and then Mission_Parameters_Defined;

   function Operating_Mode return Navigation_Option_Type with
     Pre    => Power_On
     and then Mission_Parameters_Defined;

   function Navigation_Parameters return Navigation_Parameters_Type
   with Pre => Power_On
     and then Mission_Parameters_Defined;

   function Mission_Range_From_Navigation_Parameters
     return Current_Range_Type
   with Global =>
       (Input => Navigation_Parameter_State, Proof_In => Input_State),
     Pre => Power_On
     and then Mission_Parameters_Defined;
   --  Fetch distance from Navigation_Parameters and do the appropriate
   --  conversion.

   function Operating_Point_From_Navigation_Parameters
     return Operating_Point_Type
   with Global =>
       (Input => Navigation_Parameter_State, Proof_In => Input_State),
     Pre => Power_On
     and then Mission_Parameters_Defined;
   --  Fetch altitude and speed from Navigation_Parameters and do the
   --  appropriate conversions.

   function Mission_Range return Current_Range_Type with
     Pre    => Power_On
     and then Power_State = On
     and then On_State in INIT | RUNNING
     and then Mission_Parameters_Defined;

   function Operating_Point return Operating_Point_Type with
     Pre    =>  Power_On
     and then Power_State = On
     and then On_State in INIT | RUNNING
     and then Mission_Parameters_Defined;

   function Initial_Energy_Compatible_With_Mission return Boolean
   with
     Pre => Power_On
     and then Power_State = ON
     and then On_State = INIT
     and then Init_Completed;

   function In_Flight_Energy_Compatible_With_Mission return Boolean
   with
     Pre => Power_On
     and then Power_State = ON
     and then On_State = RUNNING
     and then Running_State = FLIGHT
     and then Current_Flight_Phase = CRUISE;

   function Current_Altitude_Close_Enough_To_ref_TakeOff return Boolean with
     Global => (Input => (Input_State, Viability_Logic_State));
   --  Return True if Current_Altitude is close enough to Altitude_ref_TakeOff

   function Current_Speed_Close_Enough_To_ref_TakeOff return Boolean with
     Global => (Input => (Input_State, Viability_Logic_State));
   --  Return True if Current_Altitude is close enough to Speed_ref_TakeOff

   function Take_Off_Over return Boolean is
     (if Operating_Mode = ALTITUDE then
         Current_Altitude_Close_Enough_To_ref_TakeOff
      else Current_Speed_Close_Enough_To_ref_TakeOff)
   with
     Pre => Power_On
     and then Power_State = ON
     and then On_State = RUNNING
     and then Running_State = TAKE_OFF
     and then Mission_Parameters_Defined;

   function Descent_Over return Boolean with
     Pre => Power_On
     and then Power_State = ON
     and then On_State = RUNNING
     and then Running_State = FLIGHT
     and then Current_Flight_Phase = DESCENT;

   function Landed return Boolean is
     (Current_Speed = 0 and Current_Altitude = 0)
   with
     Pre => Power_On
     and then Power_State = ON
     and then On_State = RUNNING
     and then Running_State = LANDING;

   function Emergency_Landing return Boolean with
     Global => Output_State;

   function Mission_Aborted_Signaled return Boolean with
     Global => Output_State;

   function Mission_Cancelled_Signaled return Boolean with
     Global => Output_State;

private

   ---------------------------------------
   -- Behavioural Specification of F_MM --
   ---------------------------------------

   procedure Read_Inputs with
   --  Read values of inputs once and for all and update the current state
     Global => (In_Out => Input_State,
                Input  => (External.State, Private_State)),
     Post =>

   --  Only update inputs when new values are received.

     (if USB_Key_Present'Old then USB_Key_Present)
     and then (if Navigation_Parameters_From_GS_Received'Old
               then Navigation_Parameters_From_GS_Received)
     and then (if Operating_Mode_From_GS_Received'Old
               then Operating_Mode_From_GS_Received)

   --  Information from CP can only be changed before take-off.

     and then (if Power_State = On and then On_State = RUNNING
               then Navigation_Mode_From_CP = Navigation_Mode_From_CP'Old
               and then Payload_Bay_Closed = Payload_Bay_Closed'Old
               and then Payload_Mass_Given = Payload_Mass_Given'Old
               and then
                 (if Payload_Mass_Given then Payload_Mass = Payload_Mass'Old)
               and then
                 (if USB_Key_Present then
                      USB_Key_Present'Old
                  and then Navigation_Parameters_From_USB_Key =
                    Navigation_Parameters_From_USB_Key'Old
                  and then Operating_Mode_From_USB_Key =
                    Operating_Mode_From_USB_Key'Old));

   procedure Write_Outputs with
   --  Compute values of outputs from the current state
     Global => (Input  => Private_State,
                Output => Output_State),
     Post   =>
       (if Power_State = ON and then On_State = ABORTED then
          Emergency_Landing
        and then Mission_Aborted_Signaled)
     and then
       (if Power_State = ON
        and then On_State = INIT
        and then Init_State = CANCELLED
        then Mission_Cancelled_Signaled);

   -------------------
   -- Tasks of F_MM --
   -------------------

   ------------------------------------------------------------
   --  Management of Navigation Modes / Options / Parameters --
   ------------------------------------------------------------

   procedure Management_Of_Navigation_Modes_Options_Parameters with
   --  Compute the value of Navigation_Mode / Options / Parameters (see 6.9.4)

     Global => (Input  => (Input_State,
                           Private_State,
                           Data.Energy_Mode_Ref_TakeOff),
                In_Out => Navigation_Parameter_State),
     Pre    => Power_On,
     Post   => Navigation_Mode =

     --  In case of conflict on the navigation mode, CP prevails over GS.

     (if Navigation_Mode_From_CP = A
      or else not Navigation_Mode_From_GS_Received
      then Navigation_Mode_From_CP

     --  If CP states the mode to RC then GS can choose the navigation mode.

      else Navigation_Mode_From_GS)

     and then
         (if Mission_Parameters_Defined then

            Operating_Mode_From_Parameters =
              (if Navigation_Mode_From_CP = A
               or else not Operating_Mode_From_GS_Received
               then Operating_Mode_From_USB_Key
               else Operating_Mode_From_GS)

              --  During take-off, the most energy efficient mode is computed once and
          --  for all from the viability tables.

          and then Operating_Mode =
            (if Operating_Mode_From_Parameters = ENERGY
             and then Power_State = ON
             and then On_State = RUNNING
             and then Running_State = TAKE_OFF
             then Data.Energy_Mode_ref_TakeOff
             else Operating_Mode_From_Parameters)

          and then Navigation_Parameters =
            (if Navigation_Mode_From_CP = A
             or else not Navigation_Parameters_From_GS_Received
             then Navigation_Parameters_From_USB_Key
             else Navigation_Parameters_From_GS));

   ---------------------------------------
   -- Operating Point Update Management --
   ---------------------------------------

   procedure Operating_Point_Update_Management with
   --  Compute the value of Operating_Point and Mission_Range


     Global => (Input  =>
                  (Private_State,
                   Navigation_Parameter_State,
                   Data.Altitude_Ref_TakeOff,
                   Data.Speed_Ref_TakeOff),
                In_Out => Operating_Point_State),
     Pre    => Power_On
       and then Mission_Parameters_Defined
       and then Power_State = ON
       and then On_State in INIT | RUNNING,

     Post   =>

         --  F_MM ensures freeze of the operating point once landing is
         --  activated.

         (if On_State = RUNNING
           and then Running_State = LANDING
          then Operating_Point = Operating_Point'Old

          --  Take-Off preset operating point (Altitude_ref_TakeOff and
          --  Speed_ref_TakeOff) (see #28).

          elsif On_State = RUNNING
           and then Running_State = TAKE_OFF
          then Operating_Point =
              Operating_Point_Type'(Altitude => Data.Altitude_ref_TakeOff,
                                    Speed    => Data.Speed_ref_TakeOff)

          --  During flight, RP mode enables modification of the operating
          --  point.

          else Operating_Point = Operating_Point_From_Navigation_Parameters)

         --  RP mode enables modification of range parameter before take-off.

    and then
       (if Navigation_Mode = RP
           and then On_State = INIT
        then Mission_Range = Mission_Range_From_Navigation_Parameters
        else Mission_Range = Mission_Range'Old);

   ------------------------------
   --  Mission_Viability_Logic --
   ------------------------------

   function Mission_Profile return Mission_Profile_Type with
     Global => Viability_Logic_State;

   function Appropriate_Tabulating_Function return Viability_Domain_Mesh_Type
   with
       Global => (Input => (Private_State,
                            Viability_Logic_State,
                            Navigation_Parameter_State));

   function Distance_With_Neighbour
     (Neighbour : Mission_Profile_Type) return Mission_Profile_Distance_Type
   with
    Global => Viability_Logic_State;
   --  Compute the distance between Mission_Profile and its Neighbour.

   function Nearest_Neighbours return Neighbour_Mission_Profiles with
     Global => (Input => (Private_State,
                          Viability_Logic_State,
                          Navigation_Parameter_State));
   --  First hypothesis: nearest neighbours have the smallest distance with
   --  Mission_profile
   --
   --  Post => Nearest_Neighbours'Result'Length = 2 * Nb_Of_Dimensions
   --  for all Neighboor_Center of Nearest_Neighbours'Result =>
   --  for all Center of Appropriate_Tabulating_Function =>
   --  if Center not in Nearest_Neighboors'Result then
   --     Distance_With_Neighbour (Neighboor_Center) < Distance_With_Neighbour (Center)
   --  end if;
   --
   --  Second hypothesis: nearest neighbours has the smallest distance in only
   --  one dimension with the Mission_Profile
   --
   --  Post => Nearest_Neighbours'Result'Length = 2 * Nb_Of_Dimensions
   --  for all Neighboor_Center of Nearest_Neighbours'Result =>
   --  Appropriate_Tabulating_Function (D, A, S).Distance <= Appropriate_Tabulating_Function (D - 1, A, S).Distance
   --  and Appropriate_Tabulating_Function (D, A, S).Distance <= Appropriate_Tabulating_Function (D + 1, A, S).Distance


   function Extract_Energy_Level_For_Neighbours
     (Neighbours : Neighbour_Mission_Profiles) return Energy_Levels
   with
    Global => Viability_Logic_State;

   function Interpolated_Energy_Level return Energy_Level_Type with
    Global => Viability_Logic_State;
   --  Compute the interpolation of the energy levels of the neighbours of
   --  Mission_Profile by distance-based averaging.

   procedure Mission_Viability_Logic with
     Global => (Input  =>
                  (Input_State,
                   Private_State,
                   Navigation_Parameter_State,
                   Operating_Point_State,
                   MMS.F_PT.Data.Payload_Mass_Grid),
                 In_Out => Viability_Logic_State),
     Pre    => Power_State = ON,
     Post   =>

   --  1. Assembling mission profile.

       Mission_Profile =
        (Mass     => Payload_Mass,
         Distance => Current_Range,
         Altitude => Current_Altitude,
         Speed    => Current_Speed)

   --  2. Selecting tabulating function that corresponds to navigation mode.

     and then Appropriate_Tabulating_Function =
         (if On_State = INIT and then Navigation_Mode = A
          then Data.Amode_Initial_Domain_Mesh
          elsif On_State = INIT and then Navigation_Mode = RP
          then Data.RPmode_Initial_Domain_Mesh
          elsif Navigation_Mode = A
          then Data.Amode_Cruise_Domain_Mesh
          else Data.RPmode_Cruise_Domain_Mesh)

   --  3. Computing the nearest neighbours of Mission_Profile in
   --  Appropriate_Tabulating_Function, and the distance of Mission_Profile to
   --  its nearest neignbours.

       and then
       (for all Neighbour_Center of Nearest_Neighbours.Neighbours =>
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
                     Neighbour_Center.Mission_Profile.S).Speed)))

   --  4. Extracting energy level for the neighbours.

   and then Extract_Energy_Level_For_Neighbours (Nearest_Neighbours).Size =
       Nearest_Neighbours.Size
   and then
       (for all I In
          1 .. Extract_Energy_Level_For_Neighbours (Nearest_Neighbours).Size =>
              Extract_Energy_Level_For_Neighbours
          (Nearest_Neighbours).Neighbours (I) =
         (if On_State = INIT and then Navigation_Mode = A
          then Data.Viability_Amode_Initial
            (M => Nearest_Neighbours.Neighbours (I).Mission_Profile.M,
             D => Nearest_Neighbours.Neighbours (I).Mission_Profile.D,
             A => Nearest_Neighbours.Neighbours (I).Mission_Profile.A,
             S => Nearest_Neighbours.Neighbours (I).Mission_Profile.S)
          elsif On_State = INIT and then Navigation_Mode = RP
          then Data.Viability_RPmode_Initial
            (M => Nearest_Neighbours.Neighbours (I).Mission_Profile.M,
             D => Nearest_Neighbours.Neighbours (I).Mission_Profile.D,
             A => Nearest_Neighbours.Neighbours (I).Mission_Profile.A,
             S => Nearest_Neighbours.Neighbours (I).Mission_Profile.S)
          elsif Navigation_Mode = A
          then Data.Viability_Amode_Cruise
            (M => Nearest_Neighbours.Neighbours (I).Mission_Profile.M,
             D => Nearest_Neighbours.Neighbours (I).Mission_Profile.D,
             A => Nearest_Neighbours.Neighbours (I).Mission_Profile.A,
             S => Nearest_Neighbours.Neighbours (I).Mission_Profile.S)
          else Data.Viability_RPmode_Cruise
            (M => Nearest_Neighbours.Neighbours (I).Mission_Profile.M,
             D => Nearest_Neighbours.Neighbours (I).Mission_Profile.D,
             A => Nearest_Neighbours.Neighbours (I).Mission_Profile.A,
             S => Nearest_Neighbours.Neighbours (I).Mission_Profile.S)))

   --  5. Compute MP's enery level by interpolation of its neighbours
   --     Set appropriate value to Interpolated_Energy_Level
          ;

   procedure Initial_Mission_Viability_Logic with
   --  Compute the value of Initial_Energy_Compatible_With_Mission. It should
   --  be computed when Init_Completed is True.

     Global => (Input  =>
                  (Input_State,
                   Private_State,
                   Navigation_Parameter_State,
                   Operating_Point_State),
                 In_Out => Viability_Logic_State),
     Pre  => Power_On
     and then Power_State = ON
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
   --  at least the Interpolated_Energy_Level plus an energy margin. When
   --  EstimatedTotalMass increases, and even more so if it increases quickly,
   --  F_MM applies greater safety margins (see #17).

     Global => (Input  =>
                  (Input_State,
                   Private_State,
                   Navigation_Parameter_State,
                   Operating_Point_State),
                 In_Out => Viability_Logic_State),
     Pre => Power_On
     and then Power_State = ON
     and then On_State = RUNNING
     and then Running_State = FLIGHT
     and then Current_Flight_Phase = CRUISE;

   ---------------------------------
   -- Mission Termination Control --
   ---------------------------------

   function Current_Glide_Distance return Current_Range_Type with
    Global => Mission_Termination_State;
   --  Compute the glide distance associated with Current_Altitude using the
   --  Glide_Distance_Domain_Mesh table and the Glide_Distance tabulated
   --  function.

   procedure Mission_Termination_Control with
   --  Monitor Current_Range and activate landing.

     Global => (Input  =>
                  (Input_State,
                   Private_State,
                   Navigation_Parameter_State,
                   Operating_Point_State),
                 In_Out => Mission_Termination_State),

     Pre  => Power_On
     and then Power_State = ON
     and then On_State = RUNNING
     and then Running_State = FLIGHT
     and then Current_Flight_Phase = DESCENT,
     Post => Descent_Over =
       (Mission_Range - Current_Range < Current_Glide_Distance);

   --------------------------------
   -- Update the State Automaton --
   --------------------------------

   procedure Update_States with
     Global => (Input  =>
                  (Input_State,
                   Output_State,
                   Navigation_Parameter_State,
                   Operating_Point_State,
                   Viability_Logic_State,
                   Mission_Termination_State),
                In_Out => Private_State),
     Pre            =>
       (if Power_On and then Power_State = On and then On_State = RUNNING then
          Init_Completed),
     Contract_Cases =>
       (not Power_On
        =>
          Power_State = OFF,

        Power_State = OFF
        and then Power_On
        =>
          Power_State = ON
        and then On_State = INIT
        and then Init_State = PREPARATION,

        Power_State = ON
        and then Power_On
        and then (On_State in INIT | RUNNING)
        and then Mission_Abort_Received
        =>
          Power_State = ON
        and then On_State = ABORTED
        and then Aborted_For_Energy_Reasons = False,

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
        and then Initial_Energy_Compatible_With_Mission
        =>
          Power_State = ON
        and then On_State = INIT
        and then Init_State = READY,

        Power_State = ON
        and then Power_On
        and then On_State = INIT
        and then not Mission_Abort_Received
        and then Init_Completed
        and then Start_Or_Go_Received
        and then Initial_Energy_Compatible_With_Mission
        =>
          Power_State = ON
        and then On_State = RUNNING
        and then Running_State = TAKE_OFF,

        Power_State = ON
        and then Power_On
        and then On_State = INIT
        and then not Mission_Abort_Received
        and then Init_Completed
        and then not Initial_Energy_Compatible_With_Mission
        =>
          Power_State = ON
        and then On_State = INIT
        and then Init_State = CANCELLED,

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
        and then Current_Flight_Phase = CRUISE
        and then not In_Flight_Energy_Compatible_With_Mission
        =>
          Power_State = ON
        and then On_State = ABORTED
        and then Aborted_For_Energy_Reasons = True
        and then Mission_Aborted_Signaled
        and then Emergency_Landing,

        Power_State = ON
        and then On_State = RUNNING
        and then Running_State = FLIGHT
        and then Power_On
        and then not Mission_Abort_Received
        and then Current_Flight_Phase = DESCENT
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
          (if Current_Flight_Phase = CRUISE then
              In_Flight_Energy_Compatible_With_Mission
           elsif Current_Flight_Phase = DESCENT then not Descent_Over)
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
        -- ??? Should we verify that the distance

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

   function Operating_Mode_From_USB_Key return Navigation_Option_Type is
     (State.Input_USB_Key.Content.Navigation_Option);

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
     (State.Input_USB_Key.Content.Navigation_Parameters);

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

   ------------------------------------
   --  Definitions of Internal State --
   ------------------------------------

   function Power_State return Power_State_Type is
     (State.Power_State);

   function On_State return On_State_Type is
     (State.On_State);

   function Running_State return Running_State_Type is
     (State.Running_State);

   function Init_State return Init_State_Type is
     (State.Init_State);

   function Aborted_For_Energy_Reasons return Boolean is
     (State.Aborted_For_Energy_Reasons);

   function Descent_Over return Boolean is
     (State.Descent_Over);

   function Navigation_Parameters return Navigation_Parameters_Type is
     (State.Navigation_Parameters);

   function Navigation_Mode return Navigation_Mode_Type is
     (State.Navigation_Mode);

   function Operating_Mode_From_Parameters return Navigation_Option_Type is
     (State.Operating_Mode_From_Parameters);

   function Operating_Mode return Navigation_Option_Type is
     (State.Operating_Mode);

   function Initial_Energy_Compatible_With_Mission return Boolean is
     (State.Initial_Energy_Compatible_With_Mission);

   function In_Flight_Energy_Compatible_With_Mission return Boolean is
     (State.In_Flight_Energy_Compatible_With_Mission);

   function Mission_Range return Current_Range_Type is
     (State.Mission_Range);

   function Operating_Point return Operating_Point_Type  is
     (State.Operating_Point);

end MMS.F_PT.F_MM.Behavior;
