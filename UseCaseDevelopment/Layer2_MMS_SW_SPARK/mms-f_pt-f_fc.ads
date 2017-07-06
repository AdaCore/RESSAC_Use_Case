with Types; use Types;

package MMS.F_PT.F_FC with
  Abstract_State => (Input_State,
                     Trajectory_State,
                     Private_State,
                     Mutual_Exclusion_State,
                     Gain_Scheduling_State,
                     Propulsion_State,
                     Braking_State,
                     Output_State)
is
   pragma Elaborate_Body (MMS.F_PT.F_FC);

   type Flight_Cell_Center_Type is record
      Speed    : Current_Speed_Type;
      Altitude : Current_Altitude_Type;
   end record;

   type Flight_Speed_Center is new Positive;
   type Flight_Altitude_Center is new Positive;

   type Flight_Domain_Mesh_Type is array
     (Flight_Speed_Center range <>, Flight_Altitude_Center range <>)
     of Flight_Cell_Center_Type;

   type Gain_Type is new Integer; --  ??? some bounds

   type Gain_Triple is record
      Kd : Gain_Type;
      Kp : Gain_Type;
      Ki : Gain_Type;
   end record;

   type Time_Type is new Integer; --  in s ??? some bounds

   type Mission_State_Type is (INIT, FLIGHT, LANDING, ABORTED, COMPLETE);

   type Engine_State_Type is
     (PROPULSION, WAITING_BRAK, BRAKING, WAITING_PROP);

end MMS.F_PT.F_FC;
