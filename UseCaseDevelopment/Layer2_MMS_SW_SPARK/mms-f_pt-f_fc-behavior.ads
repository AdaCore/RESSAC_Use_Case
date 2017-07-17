with MMS.F_PT.F_FC.Data;
with External;
with Types; use Types;
with MMS.F_PT.Data;

package MMS.F_PT.F_FC.Behavior with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);

   ------------
   -- Inputs --
   ------------

   function P return Distance_Type with Global => Input_State;
   function P_Dot return Speed_Type with Global => Input_State;
   function Q return Angle_Type with Global => Input_State;

   function Start_Take_Off return Boolean with
     Global => (Input => Input_State, Proof_In => Private_State),
     Pre    => On_State = INIT;

   function Start_Landing return Boolean with
     Global => (Input => Input_State, Proof_In => Private_State),
     Pre    => On_State = RUNNING;

   function Operating_Point return Operating_Point_Type with
     Global => Input_State;

   function Operating_Mode return Navigation_Option_Type with
     Global => Input_State;

   function Mission_Range return Current_Range_Type with
     Global => Input_State;

   function Emergency_Landing return Boolean with
     Global => Input_State;

   function Payload_Mass return Payload_Mass_Type with
     Global => Input_State;

   --------------------------
   -- Properties on Inputs --
   --------------------------

   function Operating_Point_Changed return Boolean with
     Global => Input_State;

   function Operating_Mode_Changed return Boolean with
     Global => Input_State;

   ----------------------
   -- Estimated Values --
   ----------------------

   function Q_Dot return Speed_Type with Global => Input_State;

   ------------
   -- States --
   ------------

   function On_State return On_State_Type with
     Global => Private_State;

   function Running_State return Running_State_Type with
     Global => Private_State;

   function Engine_State return Engine_State_Type with
     Global => (Input => Mutual_Exclusion_State, Proof_In => Private_State),
     Pre    => On_State = RUNNING;

   function Aborted_With_Propulsion_Available return Boolean with
     Global => Private_State,
     Pre    => On_State = ABORTED;

   ----------------
   -- Properties --
   ----------------

   --  From 6.7.3.2

   function Flight_Phase return Flight_Phase_Type with
     Global => (Input => Trajectory_State, Proof_In => Private_State),
     Pre    => On_State = RUNNING;

   function In_Safety_Envelope return Boolean is
     (case Flight_Phase is
         when CLIMB   =>
            Q_Dot in MMS.F_PT.F_FC.Data.Qdot_MinCl .. MMS.F_PT.F_FC.Data.Qdot_MaxCl
              and Q < MMS.F_PT.F_FC.Data.Q_MaxCl,
         when CRUISE  =>
            Q_Dot in MMS.F_PT.F_FC.Data.Qdot_MinCr .. MMS.F_PT.F_FC.Data.Qdot_MaxCr
              and Q > MMS.F_PT.F_FC.Data.Q_MinCr
              and P_Dot < MMS.F_PT.F_FC.Data.Pdot_MaxCr,
         when DESCENT =>
            Q_Dot in MMS.F_PT.F_FC.Data.Qdot_MinDs .. MMS.F_PT.F_FC.Data.Qdot_MaxDs
              and Q < MMS.F_PT.F_FC.Data.Q_MaxDs)
   with Pre => On_State = RUNNING;

   function Selected_Option return Speed_Or_Altitude with
     Global => (Input    => Operating_Mode_State,
                Proof_In => Private_State),
     Pre    => On_State = RUNNING;

   function Already_Running return Boolean with
     Global => Private_State,
     Pre    => On_State = RUNNING;

   function Time_Since_In_Safety_Escape return Time_Type with
     Global => (Input    => Safety_Escape_State,
                Proof_In => (Input_State, Trajectory_State, Private_State)),
     Pre    => On_State = RUNNING and then not In_Safety_Envelope;

   function Fast_Evolving_Safety_Escape return Boolean with
     Global => (Input    => Private_State,
                Proof_In => (Input_State, Trajectory_State)),
     Pre    => On_State = RUNNING and then not In_Safety_Envelope;

   function Time_Since_Stopped return Time_Type with
     Global => (Input => Mutual_Exclusion_State, Proof_In => Private_State),
     Pre    => On_State = RUNNING;

   function Propulsion_Torque return Torque_Type with
     Global => Propulsion_State;

   function Braking_Torque return Torque_Type with
     Global => Braking_State;

   function Current_Speed return Current_Speed_Type with
     Global => AV_State_Vector_State;

   function Current_Altitude return Current_Altitude_Type with
     Global => AV_State_Vector_State;

   function Current_Range return Current_Range_Type with
     Global => AV_State_Vector_State;

   function Estimated_Total_Mass return Estimated_Total_Mass_Type with
     Global => AV_State_Vector_State;

   ---------------------------------------
   -- Behavioural Specification of F_FC --
   ---------------------------------------

   procedure Read_Inputs with
   --  Read values of inputs once and for all and update the current state

     Post   => Operating_Point_Changed =
       (Operating_Point'Old /= Operating_Point)
     and then Operating_Mode_Changed =
       (Operating_Mode'Old /= Operating_Mode);

   procedure Write_Outputs with
   --  Compute values of outputs from the current state

     Global => (Input  => (Input_State,
                           Trajectory_State,
                           Private_State,
                           Mutual_Exclusion_State,
                           Gain_Scheduling_State,
                           Propulsion_State,
                           Braking_State),
                Output => Output_State);

   -----------------------
   -- Safety Objectives --
   -----------------------
   --  See 7.7.3.2.E

   procedure Check_Safety_Escape with
     Global => (Input  => (Input_State, Trajectory_State),
                In_Out => Safety_Escape_State),
     Pre    => On_State in RUNNING,
     Post   =>

       --  Time_Since_In_Safety_Escape is the number of seconds since the first
       --  occurrence of safety escapes.

       (if not In_Safety_Envelope then
          (if In_Safety_Envelope'Old then Time_Since_In_Safety_Escape = 0
           else Time_Since_In_Safety_Escape > Time_Since_In_Safety_Escape'Old));

   ---------------------------------------------
   -- Choice of Operating Mode in Energy Mode --
   ---------------------------------------------
   --  See 6.7.4 (Propulsion Control)

   procedure Choose_Operating_Mode with
   --  Two systems of two equations in (pdotA, qA), (pdotS, qS) are to be solved.
   --  First:
   --  g/L= (pdotA) ** 2 * cos(qA)
   --  Operating_Point.Altitude = L * (1 - cos(qA))
   --  Second :
   --  g/L = (pdotS) ** 2 * cos(qS)
   --  Operating_Point.Speed = pdotS * L * sin(qS)
   --  If pdotA < pdotS, option ALTITUDE is selected otherwise SPEED is
   --  selected.

     Global => (Input  => Input_State,
                In_Out => Operating_Mode_State),
     Pre    => On_State = RUNNING,
     Post   =>
       (if Already_Running
        and then not Operating_Mode_Changed
        and then not Operating_Point_Changed
        then Selected_Option = Selected_Option'Old)
     and then
       (if Operating_Mode in SPEED | ALTITUDE then
          Selected_Option = Operating_Mode);

   -----------------------
   -- AV's State Vector --
   -----------------------

   procedure AV_State_Vector with
     Global => (Input  => Input_State,
                In_Out => AV_State_Vector_State);
   --  The AV's state vector is defined by:
   --  Current_Speed = L * sin(Q) * P_Dot
   --  Current_Altitude = L * (1 - cos(Q))
   --  Current_Range = Integral of Current_Speed over time
   --  Estimated_Total_Mass = M0 + Payload_Mass + Micing, where Micing is an
   --  unknown perturbation to be estimated.

   --------------------------------
   -- Update the State Automaton --
   --------------------------------
   --  Same states as F_MM except On/Off is not considered here

   procedure Update_State with
     Global         => (Input  => (Input_State, Trajectory_State),
                        In_Out => Private_State),
     Contract_Cases =>
       ((On_State in INIT .. RUNNING)
        and then Emergency_Landing
        =>
          On_State = ABORTED
        and then not Aborted_With_Propulsion_Available,

        On_State = INIT
        and then not Emergency_Landing
        and then Start_Take_Off
        =>
          On_State = RUNNING
        and then Running_State = TAKE_OFF,

        On_State = INIT
        and then not Emergency_Landing
        and then not Start_Take_Off
        =>
          On_State = INIT,

        On_State = RUNNING
        and then not Emergency_Landing
        and then
          (if Running_State = LANDING then
              not (P_Dot = 0.0 and then Q_Dot = 0.0))
        and then not In_Safety_Envelope
        and then Time_Since_In_Safety_Escape > MMS.F_PT.F_FC.Data.Escape_Time
        =>
          On_State = ABORTED
        and then Aborted_With_Propulsion_Available,

        On_State = RUNNING
        and then not Emergency_Landing
        and then
          (if Running_State = LANDING then
              not (P_Dot = 0.0 and then Q_Dot = 0.0))
        and then
          (if Running_State = FLIGHT then not Start_Landing)
        and then
          (if Running_State = TAKE_OFF then not Operating_Point_Changed)
        and then
          (if not In_Safety_Envelope then
               Time_Since_In_Safety_Escape <= MMS.F_PT.F_FC.Data.Escape_Time)
        =>
          On_State = RUNNING
        and then Running_State = Running_State'Old,

        On_State = RUNNING
        and then not Emergency_Landing
        and then Running_State = TAKE_OFF
        and then Operating_Point_Changed
        and then
          (if not In_Safety_Envelope then
               Time_Since_In_Safety_Escape <= MMS.F_PT.F_FC.Data.Escape_Time)
        =>

        --  A change in the Operating_Point means that the Take_Off phase is
        --  over, see #28.

          On_State = RUNNING
        and then Running_State = FLIGHT,


        On_State = RUNNING
        and then not Emergency_Landing
        and then Running_State = FLIGHT
        and then Start_Landing
        and then
          (if not In_Safety_Envelope then
               Time_Since_In_Safety_Escape <= MMS.F_PT.F_FC.Data.Escape_Time)
        =>
          On_State = RUNNING
        and then Running_State = LANDING,

        On_State = RUNNING
        and then not Emergency_Landing
        and then Running_State = LANDING
        and then P_Dot = 0.0 and then Q_Dot = 0.0
        =>
          On_State = COMPLETE,

        (On_State in COMPLETE | ABORTED)
        =>
          On_State = On_State'Old),

     Post           =>
       (if On_State = RUNNING then
          Engine_State = Engine_State'Old  -- ??? Needed due to current limitation in proof tool
        and then Already_Running = (On_State'Old = RUNNING));

   -------------------------------------------
   -- Propulsion / Braking Mutual Exclusion --
   -------------------------------------------

   subtype Propulsion_State_Type is Engine_State_Type
   range PROPULSION .. WAITING_BRAK;

   subtype Braking_State_Type is Engine_State_Type
   range BRAKING .. WAITING_PROP;

   function Go_To_Braking return Boolean is
     (not In_Safety_Envelope
      and then
        (Time_Since_In_Safety_Escape > MMS.F_PT.F_FC.Data.Hazard_Duration
         or else Fast_Evolving_Safety_Escape))
   with
     Pre    => On_State = RUNNING;

   function Go_To_Propulsion return Boolean is
     (In_Safety_Envelope)
   with
     Pre    => On_State = RUNNING;

   procedure Propulsion_Braking_Mutual_Exclusion with
     Global => (Input  => (Input_State, Trajectory_State, Private_State),
                In_Out => Mutual_Exclusion_State),
     Pre    => On_State = RUNNING,
     Contract_Cases =>
       (not Already_Running
        =>
          Engine_State = PROPULSION,

        --  6.7.4 Propulsion braking mutual exclusion

        Already_Running
        and then Engine_State = PROPULSION
        and then Go_To_Braking
        =>
          Engine_State = WAITING_BRAK
        and then Time_Since_Stopped = 0,

        Already_Running
        and then Engine_State = PROPULSION
        and then not Go_To_Braking
        =>
          Engine_State = PROPULSION,

        Already_Running
        and then Engine_State = BRAKING
        and then Go_To_Propulsion
        =>
          Engine_State = WAITING_PROP
        and then Time_Since_Stopped = 0,

        Already_Running
        and then Engine_State = BRAKING
        and then not Go_To_Propulsion
        =>
          Engine_State = BRAKING,

        Already_Running
        and then Engine_State = WAITING_PROP
        and then Go_To_Braking
        =>
          Engine_State = BRAKING,

        Already_Running
        and then Engine_State = WAITING_PROP
        and then not Go_To_Braking
        =>
          (if Time_Since_Stopped > MMS.F_PT.F_FC.Data.Commutation_Duration
           then Engine_State = PROPULSION
           else Engine_State = WAITING_PROP
             and then Time_Since_Stopped > Time_Since_Stopped'Old),

        Already_Running
        and then Engine_State = WAITING_BRAK
        and then Go_To_Propulsion
        =>
          Engine_State = PROPULSION,

        Already_Running
        and then Engine_State = WAITING_BRAK
        and then not Go_To_Propulsion
        =>
          (if Time_Since_Stopped > MMS.F_PT.F_FC.Data.Commutation_Duration
           then Engine_State = BRAKING
           else Engine_State = WAITING_BRAK
             and then Time_Since_Stopped > Time_Since_Stopped'Old));

   --------------------------------------
   -- Reference Trajectory Computation --
   --------------------------------------

   function Use_Set_Point_Altitude return Boolean is
     (Running_State = LANDING or else Selected_Option = ALTITUDE)
   with Pre => On_State = RUNNING;

   function Set_Point_Altitude return Current_Altitude_Type with
     Global => (Input    => Trajectory_State,
                Proof_In => (Input_State,
                             Private_State,
                             Operating_Mode_State,
                             Mutual_Exclusion_State)),
     Pre    => On_State = RUNNING
     and then Use_Set_Point_Altitude;

   function Intermediate_Set_Point_Altitude return Current_Altitude_Type with
     Global => (Input    => Trajectory_State,
                Proof_In => (Input_State,
                             Private_State,
                             Operating_Mode_State,
                             Mutual_Exclusion_State)),
     Pre    => On_State = RUNNING
     and then Use_Set_Point_Altitude;

   function Close_To_Set_Point_Altitude return Boolean with
     Global => (Input    => Trajectory_State,
                Proof_In => (Input_State,
                             Private_State,
                             Operating_Mode_State,
                             Mutual_Exclusion_State)),
     Pre    => On_State = RUNNING
     and then Use_Set_Point_Altitude;
   --  True if we are close enough to the set point. Used to avoid Zeno effect.

   function Intermediate_Set_Point_Altitude_Reached return Boolean with
     Global => (Input    => Trajectory_State,
                Proof_In => (Input_State,
                             Private_State,
                             Operating_Mode_State,
                             Mutual_Exclusion_State)),
     Pre    => On_State = RUNNING
     and then Already_Running
     and then not Operating_Mode_Changed
     and then not Operating_Point_Changed
     and then Use_Set_Point_Altitude;
   --  True if we have reached the previous intermediate set point.

   function Use_Set_Point_Speed return Boolean is
     (Running_State = LANDING or else Selected_Option = SPEED)
   with Pre => On_State = RUNNING;

   function Set_Point_Speed return Current_Speed_Type with
     Global => (Input    => Trajectory_State,
                Proof_In => (Input_State,
                             Private_State,
                             Operating_Mode_State,
                             Mutual_Exclusion_State)),
     Pre    => On_State = RUNNING
     and then Use_Set_Point_Speed;

   function Intermediate_Set_Point_Speed return Current_Speed_Type with
     Global => (Input    => Trajectory_State,
                Proof_In => (Input_State,
                             Private_State,
                             Operating_Mode_State,
                             Mutual_Exclusion_State)),
     Pre    => On_State = RUNNING
     and then Use_Set_Point_Speed;

   function Close_To_Set_Point_Speed return Boolean with
     Global => (Input    => Trajectory_State,
                Proof_In => (Input_State,
                             Private_State,
                             Operating_Mode_State,
                             Mutual_Exclusion_State)),
     Pre    => On_State = RUNNING
     and then Use_Set_Point_Speed;
   --  True if we are close enough to the set point. Used to avoid Zeno effect.

   function Intermediate_Set_Point_Speed_Reached return Boolean with
     Global => (Input    => Trajectory_State,
                Proof_In => (Input_State,
                             Private_State,
                             Operating_Mode_State,
                             Mutual_Exclusion_State)),
     Pre    => On_State = RUNNING
     and then Already_Running
     and then not Operating_Mode_Changed
     and then not Operating_Point_Changed
     and then Use_Set_Point_Speed;
   --  True if we have reached the previous intermediate set point.

   function Set_Point_Distance return Current_Range_Type with
     Global => (Input    => Trajectory_State,
                Proof_In => Private_State),
     Pre    => On_State = RUNNING and then Running_State = LANDING;

   function Close_To_Set_Point return Boolean is
     (if Selected_Option = ALTITUDE then Close_To_Set_Point_Altitude
      else Close_To_Set_Point_Speed)
   with Pre => On_State = RUNNING;

   procedure Reference_Trajectory_Computation with
     --  Computed at each cycle. Slower rates are possible but not too slow.

     Global => (Input  => (Input_State, Private_State, Operating_Mode_State),
                In_Out => Trajectory_State),
     Pre    => On_State = RUNNING,
     Post   =>

   --  For landing, the target, or preset operating point, or (final) reference
   --  value, is more complicated than for the other phases.
   --  For all phases except landing there is only one target: either a Speed
   --  value or an Altitude value. But for landing there are three of them:
   --  - Current_Range ~ Mission_Range, i.e (Current_Range - Mission_Range
   --     =< DeliveryPrecisionUpperBound).
   --  - Current_Altitude = 0
   --  - Current_Speed = 0
   --  (see #29)

       (if Running_State = LANDING then
          Set_Point_Altitude = 0
          and then Set_Point_Speed = 0
          and then Set_Point_Distance = Mission_Range
        else
          (case Selected_Option is
                 when ALTITUDE =>
                   Set_Point_Altitude = Operating_Point.Altitude,
                 when SPEED =>
                   Set_Point_Speed = Operating_Point.Speed))

     --  Instead of giving the true set-point to propulsion control, it gives
     --  half of the change amplitude. When current intermediate set-point is
     --  reached, a new one is computed (zeno like aspects to be addressed for
     --  convergence).
     --  Module is reset by any operating point change.

     and then
       (if Use_Set_Point_Speed then
          Intermediate_Set_Point_Speed =
          (if not Already_Running
           or else Operating_Point_Changed
           or else Operating_Mode_Changed
           or else (Intermediate_Set_Point_Speed_Reached
                    and then not Close_To_Set_Point_Speed)
           then
             (Set_Point_Speed + Current_Speed) / 2
           elsif Close_To_Set_Point_Speed then Set_Point_Speed
           else Intermediate_Set_Point_Speed'Old))

     and then
       (if Use_Set_Point_Altitude then
          Intermediate_Set_Point_Altitude =
          (if not Already_Running
           or else Operating_Point_Changed
           or else Operating_Mode_Changed
           or else (Intermediate_Set_Point_Altitude_Reached
                    and then not Close_To_Set_Point_Altitude)
           then (Set_Point_Altitude + Current_Altitude) / 2
           elsif Close_To_Set_Point_Altitude then Set_Point_Altitude
           else Intermediate_Set_Point_Altitude'Old))

    --  Changes in the operating point provoque termination of the current
    --  cruise phase and activate a transient climb or descent phase to
    --  capture the new operating point (see 6.6.4 4. Cruise).

     and then Flight_Phase =
       (if Running_State = LANDING then
          DESCENT
        elsif not Already_Running
           or else Operating_Point_Changed
           or else not Close_To_Set_Point
        then
            (if (Selected_Option = ALTITUDE
                 and then Current_Altitude < Set_Point_Altitude)
             or else
               (Selected_Option = SPEED
                and then Current_Speed < Set_Point_Speed)
             then CLIMB
             else DESCENT)
        else CRUISE);

   ---------------------
   -- Gain Scheduling --
   ---------------------

   function Mission_Profile return Mission_Profile_Type with
     Global => Gain_Scheduling_State;

   function Distance_With_Neighbour
     (Neighbour : Mission_Profile_Type) return Mission_Profile_Distance_Type
   with
    Global => Gain_Scheduling_State;
   --  Compute the distance between Mission_Profile and its Neighbour.

   function Nearest_Neighbours return Neighbour_Mission_Profiles with
    Global => Gain_Scheduling_State;

   function Extract_Gain_Triple_For_Neighbours return Gain_Triples with
    Global => Gain_Scheduling_State;

   function Interpolated_Gain_Triple return Gain_Triple with
    Global => Gain_Scheduling_State;
   --  Compute the interpolation of the energy levels of the neighbours of
   --  Mission_Profile by distance-based averaging.

   procedure Gain_Scheduling with
     Global => (Input  => (Input_State,
                           Trajectory_State,
                           Private_State,
                           Mutual_Exclusion_State),
                In_Out => Gain_Scheduling_State),
     Pre    => On_State = RUNNING
     and then Engine_State in BRAKING | PROPULSION,
     Post   =>

   --  1. Assembling mission profile.

       Mission_Profile =
        (Mass     => Payload_Mass,
         Altitude => Current_Altitude,
         Speed    => Current_Speed)

   --  2. Computing the nearest neighbours of Mission_Profile in
   --  Flight_Domain_Mesh, and the distance of Mission_Profile to its nearest
   --  neignbours.

       and then
       (for all Neighbour_Center of Nearest_Neighbours.Neighbours =>
          Neighbour_Center.Mission_Profile.M in
            MMS.F_PT.Data.Payload_Mass_Grid'Range
        and then Neighbour_Center.Mission_Profile.S in
          Data.Flight_Domain_Mesh'Range (1)
        and then Neighbour_Center.Mission_Profile.A in
          Data.Flight_Domain_Mesh'Range (2)
        and then Neighbour_Center.Distance =
          Distance_With_Neighbour
            (Mission_Profile_Type'
               (Mass     =>
                   MMS.F_PT.Data.Payload_Mass_Grid
                     (Neighbour_Center.Mission_Profile.M),
                Altitude =>
                  Data.Flight_Domain_Mesh
                    (Neighbour_Center.Mission_Profile.S,
                     Neighbour_Center.Mission_Profile.A).Altitude,
                Speed    =>
                  Data.Flight_Domain_Mesh
                    (Neighbour_Center.Mission_Profile.S,
                     Neighbour_Center.Mission_Profile.A).Speed)))

   --  3. Extracting gain triples for the neighbours.

   and then Extract_Gain_Triple_For_Neighbours.Size =
       Nearest_Neighbours.Size
   and then
       (for all I in 1 .. Extract_Gain_Triple_For_Neighbours.Size =>
          Extract_Gain_Triple_For_Neighbours.Neighbours (I) =
         (case Flight_Phase is
          when CLIMB   =>
            (if Engine_State = PROPULSION then
               Data.Climb_Propulsion_Gains
                 (M => Nearest_Neighbours.Neighbours (I).Mission_Profile.M,
                  A => Nearest_Neighbours.Neighbours (I).Mission_Profile.A,
                  S => Nearest_Neighbours.Neighbours (I).Mission_Profile.S)
             else
               Data.Climb_Braking_Gains
                 (M => Nearest_Neighbours.Neighbours (I).Mission_Profile.M,
                  A => Nearest_Neighbours.Neighbours (I).Mission_Profile.A,
                  S => Nearest_Neighbours.Neighbours (I).Mission_Profile.S)),
          when CRUISE  =>
            (if Engine_State = PROPULSION then
               Data.Cruise_Propulsion_Gains
                 (M => Nearest_Neighbours.Neighbours (I).Mission_Profile.M,
                  A => Nearest_Neighbours.Neighbours (I).Mission_Profile.A,
                  S => Nearest_Neighbours.Neighbours (I).Mission_Profile.S)
             else
               Data.Cruise_Braking_Gains
                 (M => Nearest_Neighbours.Neighbours (I).Mission_Profile.M,
                  A => Nearest_Neighbours.Neighbours (I).Mission_Profile.A,
                  S => Nearest_Neighbours.Neighbours (I).Mission_Profile.S)),
          when DESCENT =>
            (if Engine_State = PROPULSION then
               Data.Descent_Propulsion_Gains
                 (M => Nearest_Neighbours.Neighbours (I).Mission_Profile.M,
                  A => Nearest_Neighbours.Neighbours (I).Mission_Profile.A,
                  S => Nearest_Neighbours.Neighbours (I).Mission_Profile.S)
             else
               Data.Descent_Braking_Gains
                 (M => Nearest_Neighbours.Neighbours (I).Mission_Profile.M,
                  A => Nearest_Neighbours.Neighbours (I).Mission_Profile.A,
                  S => Nearest_Neighbours.Neighbours (I).Mission_Profile.S))))

   --  4. Compute MP's gain triple by interpolation of its neighbours
   --     Set appropriate value to Interpolated_Gain_Triple.
          ;

   ------------------------
   -- Propulsion Control --
   ------------------------

   function Propulsion_Altitude_Error return Current_Altitude_Type'Base is
     (Current_Altitude - Intermediate_Set_Point_Altitude)
   with Pre => On_State = RUNNING
     and then Use_Set_Point_Altitude;

   function Propulsion_Speed_Error return Current_Speed_Type'Base is
     (Current_Speed - Intermediate_Set_Point_Speed)
   with Pre => On_State = RUNNING
     and then Use_Set_Point_Speed;

   function Propulsion_Error return Error_Type with
     Global => Propulsion_State;
   --  In Flight and maybe also Take-Off phase, should be
   --  Propulsion_Altitude_Error if Selected_Mode is ALTITUDE and
   --  Propulsion_Speed_Error otherwise, but I am confused about the units...
   --  In Landing phase, should depend on speed, altitude, and also distance,
   --  see #29.

   function Propulsion_Integral_Error return Error_Type with
     Global => Propulsion_State;
   --  Cumulative past Propulsion_Error during the last Ti seconds

   function Propulsion_Derivative_Error return Error_Type with
     Global => Propulsion_State;
   --  Propulsion_Error variation at current time

   procedure Propulsion_Control with
     Global => (Input  => (Input_State,
                           Trajectory_State,
                           Private_State,
                           Gain_Scheduling_State,
                           Mutual_Exclusion_State),
                In_Out => Propulsion_State),
     Pre    => On_State = RUNNING
     and then Engine_State = PROPULSION,
     Post   => Propulsion_Torque =
       Torque_Type (Interpolated_Gain_Triple.Kp * Propulsion_Error) +
       Torque_Type (Interpolated_Gain_Triple.Ki * Propulsion_Integral_Error) +
       Torque_Type (Interpolated_Gain_Triple.Kd * Propulsion_Derivative_Error);

   ---------------------
   -- Braking Control --
   ---------------------

   function Braking_Altitude_Error return Current_Altitude_Type'Base is
     (Current_Altitude - Intermediate_Set_Point_Altitude)
   with Pre => On_State = RUNNING
     and then Use_Set_Point_Altitude;

   function Braking_Speed_Error return Current_Speed_Type'Base is
     (Current_Speed - Intermediate_Set_Point_Speed)
   with Pre => On_State = RUNNING
     and then Use_Set_Point_Speed;

   function Braking_Error return Error_Type with
     Global => Braking_State;
   --  Same as propulsion, see #28.

   function Braking_Integral_Error return Error_Type with
     Global => Braking_State;
   --  Cumulative past Braking_Error during the last Ti seconds

   function Braking_Derivative_Error return Error_Type with
     Global => Braking_State;
   --  Braking_Error variation at current time

   procedure Braking_Control with
     Global => (Input  => (Input_State,
                           Trajectory_State,
                           Private_State,
                           Gain_Scheduling_State,
                           Mutual_Exclusion_State),
                In_Out => Braking_State),
     Pre    => On_State = RUNNING
     and then Engine_State = BRAKING,
     Post   => Braking_Torque =
       Torque_Type (Interpolated_Gain_Triple.Kp * Braking_Error) +
       Torque_Type (Interpolated_Gain_Triple.Ki * Braking_Integral_Error) +
       Torque_Type (Interpolated_Gain_Triple.Kd * Braking_Derivative_Error);

end MMS.F_PT.F_FC.Behavior;
