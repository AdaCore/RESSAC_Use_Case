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
     Pre    => Mission_State = INIT;

   function Start_Landing return Boolean with
     Global => (Input => Input_State, Proof_In => Private_State),
     Pre    => Mission_State = FLIGHT;

   function Operating_Point return Operating_Point_Type with
     Global => Input_State;

   function Emergency_Landing return Boolean with
     Global => Input_State;

   --------------------------
   -- Properties on Inputs --
   --------------------------

   function Operating_Point_Changed return Boolean with
     Global => Input_State;

   ----------------------
   -- Estimated Values --
   ----------------------

   function Q_Dot return Speed_Type with Global => Input_State;

   ------------
   -- States --
   ------------

   function Mission_State return Mission_State_Type with
     Global => Private_State;

   function Engine_State return Engine_State_Type with
     Global => (Input => Mutual_Exclusion_State, Proof_In => Private_State),
     Pre    => Mission_State in FLIGHT | LANDING;

   function Aborted_With_Propulsion_Available return Boolean with
     Global => Private_State,
     Pre    => Mission_State = ABORTED;

   ----------------
   -- Properties --
   ----------------

   --  From 6.7.3.2

   function Flight_Phase return Flight_Phase_Type with
     Global => (Input => Trajectory_State, Proof_In => Private_State),
     Pre    => Mission_State = FLIGHT;

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
   with Pre => Mission_State = FLIGHT;

   function Already_Running return Boolean with
     Global => Private_State,
     Pre    => Mission_State in FLIGHT | LANDING;

   function Time_Since_In_Safety_Escape return Time_Type with
     Global => (Input    => Private_State,
                Proof_In => (Input_State, Trajectory_State)),
     Pre    => (Mission_State = FLIGHT and then not In_Safety_Envelope)
     or else Mission_State = ABORTED;

   function Fast_Evolving_Safety_Escape return Boolean with
     Global => (Input    => Private_State,
                Proof_In => (Input_State, Trajectory_State)),
     Pre    => Mission_State = FLIGHT and then not In_Safety_Envelope;

   function Time_Since_Stopped return Time_Type with
     Global => (Input => Mutual_Exclusion_State, Proof_In => Private_State),
     Pre    => Mission_State in FLIGHT | LANDING;

   function Propulsion_Torque return Torque_Type with
     Global => (Input => Propulsion_State, Proof_In => Private_State);

   function Braking_Torque return Torque_Type with
     Global => (Input => Braking_State, Proof_In => Private_State);

   ---------------------------------------
   -- Behavioural Specification of F_FC --
   ---------------------------------------

   procedure Read_Inputs with
   --  Read values of inputs once and for all and update the current state

     Post   => Operating_Point_Changed =
       (Operating_Point'Old /= Operating_Point);

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

   procedure Update_State with
     Global         => (Input  => (Input_State, Trajectory_State),
                        In_Out => Private_State),
     Contract_Cases =>

       --  ??? All these are guesses...

       ((Mission_State in INIT .. LANDING)
        and then Emergency_Landing
        =>
          Mission_State = ABORTED
        and then not Aborted_With_Propulsion_Available,

        Mission_State = INIT
        and then not Emergency_Landing
        and then Start_Take_Off
        =>
          Mission_State = FLIGHT,

        Mission_State = INIT
        and then not Emergency_Landing
        and then not Start_Take_Off
        =>
          Mission_State = INIT,

        Mission_State = FLIGHT
        and then not Emergency_Landing
        and then Start_Landing
        =>
          Mission_State = LANDING,

        Mission_State = FLIGHT
        and then not Emergency_Landing
        and then not Start_Landing
        =>
          (if Time_Since_In_Safety_Escape > MMS.F_PT.F_FC.Data.Escape_Time then
               Mission_State = ABORTED
             and then Aborted_With_Propulsion_Available
           else Mission_State = FLIGHT),

        Mission_State = LANDING
        and then not Emergency_Landing
        =>
          (if P_Dot = 0.0 and then Q_Dot = 0.0 then
               Mission_State = COMPLETE
           else Mission_State = LANDING),

        (Mission_State in COMPLETE | ABORTED)
        =>
          Mission_State = Mission_State'Old),

     Post           =>
       (if Mission_State in FLIGHT | LANDING then
          Engine_State = Engine_State'Old  -- ??? Needed due to current limitation in proof tool
        and then Already_Running = (Mission_State'Old in FLIGHT | LANDING))

       --  Time_Since_In_Safety_Escape is the number of seconds since the first
       --  occurrence of safety escapes.

     and then
       (if not In_Safety_Envelope then
          (if In_Safety_Envelope'Old then Time_Since_In_Safety_Escape = 0
           else Time_Since_In_Safety_Escape > Time_Since_In_Safety_Escape'Old));

   function Go_To_Braking return Boolean is
     (Mission_State = LANDING
      or else
        (not In_Safety_Envelope
         and then
           (Time_Since_In_Safety_Escape > MMS.F_PT.F_FC.Data.Hazard_Duration
            or else Fast_Evolving_Safety_Escape)))
   with
     Pre    => Mission_State in FLIGHT | LANDING;

   function Go_To_Propulsion return Boolean is
     (Mission_State = FLIGHT and then In_Safety_Envelope)
   with
     Pre    => Mission_State in FLIGHT | LANDING;

   procedure Propulsion_Braking_Mutual_Exclusion with
     Global => (Input  => (Input_State, Trajectory_State, Private_State),
                In_Out => Mutual_Exclusion_State),
     Pre    => Mission_State in FLIGHT | LANDING,
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

   procedure Reference_Trajectory_Computation with
     Global => (Input  => (Input_State, Private_State),
                In_Out => Trajectory_State),
     Pre    => Mission_State in FLIGHT .. LANDING,
     Post   =>

       --  Changes in the operating point provoque termination of the current
       --  cruise phase and activate a transient climb or descent phase to
       --  capture the new operating point (see 6.6.4 4. Cruise).

       (if Mission_State = FLIGHT and then Operating_Point_Changed then
          Flight_Phase in CLIMB | DESCENT);

   procedure Gain_Scheduling with
     Global => (Input  => (Input_State,
                           Trajectory_State,
                           Private_State,
                           Mutual_Exclusion_State),
                In_Out => Gain_Scheduling_State),
     Pre    => Mission_State in FLIGHT .. LANDING;

   procedure Propulsion_Control with
     Global => (Input  => (Input_State,
                           Trajectory_State,
                           Private_State,
                           Gain_Scheduling_State,
                           Mutual_Exclusion_State),
                In_Out => Propulsion_State),
     Pre    => Mission_State = FLIGHT and then Engine_State = PROPULSION;

   procedure Braking_Control with
     Global => (Input  => (Input_State,
                           Trajectory_State,
                           Private_State,
                           Gain_Scheduling_State,
                           Mutual_Exclusion_State),
                In_Out => Braking_State),
     Pre    => Mission_State in FLIGHT .. LANDING
     and then Engine_State = BRAKING;

end MMS.F_PT.F_FC.Behavior;
