with Types; use Types;

package MMS.F_PT.F_FC with
  Abstract_State => (Input_State,
                     Safety_Escape_State,
                     Operating_Mode_State,
                     AV_State_Vector_State,
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

   subtype Error_Type is Gain_Type'Base; --  ??? what is the type of a PID error?

   type Time_Type is new Integer; --  in s ??? some bounds

   type Engine_State_Type is
     (PROPULSION, WAITING_BRAK, BRAKING, WAITING_PROP);

   type Mission_Profile_Type is record
      Mass     : Payload_Mass_Type;
      Altitude : Current_Altitude_Type;
      Speed    : Current_Speed_Type;
   end record;

   type Center_Mission_Profile_Type is record
      M : Payload_Mass_Center;
      A : Flight_Altitude_Center;
      S : Flight_Speed_Center;
   end record;

   type Mission_Profile_Distance_Type is new Natural;

   type Neighbour_Mission_Profile_Type is record
      Mission_Profile : Center_Mission_Profile_Type;
      Distance        : Mission_Profile_Distance_Type;
   end record;

   type Num_Of_Neighbours is new Positive range 1 .. 8;

   type Neighbour_Mission_Profile_Array_Type is array
     (Num_Of_Neighbours range <>)
     of Neighbour_Mission_Profile_Type;

   type Neighbour_Mission_Profiles (Size : Num_Of_Neighbours) is record
     Neighbours : Neighbour_Mission_Profile_Array_Type (1 .. Size);
   end record;

   type Gain_Triple_Array_Type is array (Num_Of_Neighbours range <>)
     of Gain_Triple;

   type Gain_Triples (Size : Num_Of_Neighbours) is record
     Neighbours : Gain_Triple_Array_Type (1 .. Size);
   end record;

end MMS.F_PT.F_FC;
