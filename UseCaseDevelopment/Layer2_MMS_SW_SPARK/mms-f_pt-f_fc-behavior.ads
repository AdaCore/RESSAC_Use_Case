with MMS.F_PT.F_FC.Data;
with External;
with Types; use Types;

package MMS.F_PT.F_FC.Behavior with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);

   ------------
   -- Inputs --
   ------------

   function P return Distance_Type with Global => Private_State;
   function P_Dot return Speed_Type with Global => Private_State;
   function Q return Angle_Type with Global => Private_State;

   function Start_Take_Off return Boolean with
     Global => Private_State,
     Pre    => Mission_State = INIT;

   function Start_Landing return Boolean with
     Global => Private_State,
     Pre    => Mission_State = RUNNING;

   function Operating_Point return Operating_Point_Type with
     Global => Private_State;

   --------------------------
   -- Properties on Inputs --
   --------------------------

   function Operating_Point_Changed return Boolean with
     Global => Private_State;

   ----------------------
   -- Estimated Values --
   ----------------------

   function Q_Dot return Angular_Speed_Type with Global => Private_State;

   ------------
   -- States --
   ------------

   type Mission_State_Type is (INIT, RUNNING, ABORTED, COMPLETE);

   function Mission_State return Mission_State_Type with
     Global => Private_State;

   function Flight_Phase_State return Flight_Phase_Type with
     Global => Private_State,
     Pre    => Mission_State = RUNNING;

   type Engine_State_Type is
     (PROPULSION, WAITING_BRAK, BRAKING, WAITING_PROP);

   function Engine_State return Engine_State_Type with
     Global => Private_State,
     Pre    => Mission_State = RUNNING;

   ----------------
   -- Properties --
   ----------------

   --  From 6.7.3.2

   function In_Safety_Envelope return Boolean is
     (case Flight_Phase_State is
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
   with Pre => Mission_State = RUNNING;

   function Time_Since_In_Safety_Escape return Time_Type with
     Global => Private_State,
     Pre    => (Mission_State = RUNNING and then not In_Safety_Envelope)
     or else Mission_State = ABORTED;

   function Fast_Evolving_Safety_Escape return Boolean with
     Global => Private_State,
     Pre    => Mission_State = RUNNING and then not In_Safety_Envelope;

   function Time_Since_Stopped return Time_Type with
     Global => Private_State,
     Pre    => Mission_State = RUNNING;

   ---------------------------------------
   -- Behavioural Specification of F_FC --
   ---------------------------------------

   procedure Read_Inputs with
   --  Read values of inputs once and for all and update the current state
     Global => (In_Out => Private_State,
                Input  => External.State),
     Post   => Operating_Point_Changed =
       (Operating_Point'Old /= Operating_Point);

   procedure Write_Outputs with
   --  Compute values of outputs from the current state
     Global => (Input  => Private_State,
                Output => Output_State);

   procedure Run with
     Global         => (In_Out => Private_State),
     Contract_Cases =>

       --  ??? All these are guesses...

       (Mission_State = INIT
        and then Start_Take_Off
        =>
          Mission_State = RUNNING
        and then Engine_State = PROPULSION,

        Mission_State = INIT
        and then not Start_Take_Off
        =>
          Mission_State = INIT,

        Mission_State = RUNNING
        and then Start_Landing
        =>
          Mission_State = COMPLETE,

        Mission_State = RUNNING
        and then not Start_Landing
        =>
          (if Time_Since_In_Safety_Escape > MMS.F_PT.F_FC.Data.Escape_Time then
               Mission_State = ABORTED
           else Mission_State = RUNNING),

        (Mission_State in COMPLETE | ABORTED)
        =>
          Mission_State = Mission_State'Old),

     Post           =>

       --  Changes in the operating point provoque termination of the current
       --  cruise phase and activate a transient climb or descent phase to
       --  capture the new operating point (see 6.6.4 4. Cruise).

       (if Operating_Point_Changed then Flight_Phase_State in CLIMB | DESCENT)

       --  Time_Since_In_Safety_Escape is the number of seconds since the first
       --  occurrence of safety escapes.

       and then
       (if not In_Safety_Envelope then
          (if In_Safety_Envelope'Old then Time_Since_In_Safety_Escape = 0
           else Time_Since_In_Safety_Escape > Time_Since_In_Safety_Escape'Old))

        --  6.7.4 Propulsion braking mutual exclusion

       and then
       (if Mission_State = RUNNING and then Mission_State'Old = RUNNING then
          (case Engine_State'Old is
           when PROPULSION   =>
             (if not In_Safety_Envelope
              and then
                (Time_Since_In_Safety_Escape > MMS.F_PT.F_FC.Data.Hazard_Duration
                 or else Fast_Evolving_Safety_Escape)
              then Engine_State = WAITING_BRAK
                and then Time_Since_Stopped = 0
              else Engine_State = PROPULSION),

           when BRAKING      =>
             (if In_Safety_Envelope
              then Engine_State = WAITING_PROP
                and then Time_Since_Stopped = 0
              else Engine_State = BRAKING),

           when WAITING_PROP =>
             (if not In_Safety_Envelope
              and then (Time_Since_In_Safety_Escape > MMS.F_PT.F_FC.Data.Hazard_Duration
                        or else Fast_Evolving_Safety_Escape)
              then Engine_State = BRAKING
              elsif Time_Since_Stopped > MMS.F_PT.F_FC.Data.Commutation_Duration
              then Engine_State = PROPULSION
              else Engine_State = WAITING_PROP
                and then Time_Since_Stopped > Time_Since_Stopped'Old),

           when WAITING_BRAK =>
               (if In_Safety_Envelope
                then Engine_State = PROPULSION
                elsif Time_Since_Stopped > MMS.F_PT.F_FC.Data.Commutation_Duration
                then Engine_State = BRAKING
                else Engine_State = WAITING_BRAK
                  and then Time_Since_Stopped > Time_Since_Stopped'Old)));

end MMS.F_PT.F_FC.Behavior;
