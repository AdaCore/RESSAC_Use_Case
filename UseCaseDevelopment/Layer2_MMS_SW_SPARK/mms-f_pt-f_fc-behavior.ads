with MMS.F_PT.F_FC.Data;
with External;
with Types; use Types;

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

   function Selected_Option return Navigation_Option_Type with
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
     Post   => Selected_Option in SPEED | ALTITUDE
     and then
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
          On_State = RUNNING,

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
          (if not In_Safety_Envelope then
               Time_Since_In_Safety_Escape <= MMS.F_PT.F_FC.Data.Escape_Time)
        =>
          On_State = RUNNING,

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

   function Set_Point_Altitude return Current_Altitude_Type with
     Global => (Input    => Trajectory_State,
                Proof_In => Private_State),
     Pre    => On_State = RUNNING;

   function Intermediate_Set_Point_Altitude return Current_Altitude_Type with
     Global => (Input    => Trajectory_State,
                Proof_In => Private_State),
     Pre    => On_State = RUNNING;

   function Close_To_Set_Point_Altitude return Boolean with
     Global => (Input    => Trajectory_State,
                Proof_In => Private_State),
     Pre    => On_State = RUNNING;
   --  True if we are close enough to the set point. Used to avoid Zeno effect.

   function Intermediate_Set_Point_Altitude_Reached return Boolean with
     Global => (Input    => Trajectory_State,
                Proof_In => Private_State),
     Pre    => On_State = RUNNING and then Already_Running;
   --  True if we have reached the previous intermediate set point.

   function Set_Point_Speed return Current_Speed_Type with
     Global => (Input    => Trajectory_State,
                Proof_In => Private_State),
     Pre    => On_State = RUNNING;

   function Intermediate_Set_Point_Speed return Current_Speed_Type with
     Global => (Input    => Trajectory_State,
                Proof_In => Private_State),
     Pre    => On_State = RUNNING;

   function Close_To_Set_Point_Speed return Boolean with
     Global => (Input    => Trajectory_State,
                Proof_In => Private_State),
     Pre    => On_State = RUNNING;
   --  True if we are close enough to the set point. Used to avoid Zeno effect.

   function Intermediate_Set_Point_Speed_Reached return Boolean with
     Global => (Input    => Trajectory_State,
                Proof_In => Private_State),
     Pre    => On_State = RUNNING and then Already_Running;
   --  True if we have reached the previous intermediate set point.

   function Set_Point_Distance return Current_Range_Type with
     Global => (Input    => Trajectory_State,
                Proof_In => Private_State),
     Pre    => On_State = RUNNING and then Running_State = LANDING;

   procedure Reference_Trajectory_Computation with
     --  Computed at each cycle. Slower rates are possible but not too slow.

     Global => (Input  => (Input_State, Private_State),
                In_Out => Trajectory_State),
     Pre    => On_State = RUNNING,
     Post   =>

       --  For landing, a distance objective is added to the zero-altitude
       --  objective. Landing must occur at range completion.
       --  ??? How is it used by the PID?

       (if Running_State = FLIGHT then
          Set_Point_Altitude = Operating_Point.Altitude
          and then Set_Point_Speed = Operating_Point.Speed
        else
          Set_Point_Altitude = 0
          and then Set_Point_Speed = 0
          and then Set_Point_Distance = Mission_Range)

     --  Instead of giving the true set-point to propulsion control, it gives
     --  half of the change amplitude. When current intermediate set-point is
     --  reached, a new one is computed (zeno like aspects to be addressed for
     --  convergence.
     --  Module is reset by any operating point change.

     and then
       (if not Already_Running
        or else (Running_State = FLIGHT and then Operating_Point_Changed)
        or else (Intermediate_Set_Point_Speed_Reached
                 and then not Close_To_Set_Point_Speed) then
          Intermediate_Set_Point_Speed =
                (Set_Point_Speed + Current_Speed) / 2
        elsif Close_To_Set_Point_Speed then
          Intermediate_Set_Point_Speed = Set_Point_Speed
        else Intermediate_Set_Point_Speed = Intermediate_Set_Point_Speed'Old)

     and then
       (if not Already_Running
        or else (Running_State = FLIGHT and then Operating_Point_Changed)
        or else (Intermediate_Set_Point_Altitude_Reached
                 and then not Close_To_Set_Point_Altitude) then
          Intermediate_Set_Point_Altitude =
                (Set_Point_Altitude + Current_Altitude) / 2
        elsif Close_To_Set_Point_Altitude then
          Intermediate_Set_Point_Altitude = Set_Point_Altitude
        else Intermediate_Set_Point_Altitude = Intermediate_Set_Point_Altitude'Old)

    --  Changes in the operating point provoque termination of the current
    --  cruise phase and activate a transient climb or descent phase to
    --  capture the new operating point (see 6.6.4 4. Cruise).
    --  ??? How is the current Flight_Phase computed ?

     and then
       (if Running_State = FLIGHT and then Operating_Point_Changed then
          Flight_Phase in CLIMB | DESCENT);

   procedure Gain_Scheduling with
     Global => (Input  => (Input_State,
                           Trajectory_State,
                           Private_State,
                           Mutual_Exclusion_State),
                In_Out => Gain_Scheduling_State),
     Pre    => On_State = RUNNING;

   procedure Propulsion_Control with
     Global => (Input  => (Input_State,
                           Trajectory_State,
                           Private_State,
                           Gain_Scheduling_State,
                           Mutual_Exclusion_State),
                In_Out => Propulsion_State),
     Pre    => On_State = RUNNING
     and then Engine_State = PROPULSION;

   procedure Braking_Control with
     Global => (Input  => (Input_State,
                           Trajectory_State,
                           Private_State,
                           Gain_Scheduling_State,
                           Mutual_Exclusion_State),
                In_Out => Braking_State),
     Pre    => On_State = RUNNING
     and then Engine_State = BRAKING;

end MMS.F_PT.F_FC.Behavior;
