with Types; use Types;

package MMS.F_PT.F_MM with
  SPARK_Mode,
  Abstract_State =>
    (Navigation_Parameter_State,
     Operating_Point_State,
     Viability_Logic_State,
     Mission_Termination_State,
     Private_State,
     Output_State,
     Input_State)
is
   pragma Elaborate_Body (MMS.F_PT.F_MM);

   type Power_State_Type is (ON, OFF);

   type Init_State_Type is (PREPARATION, READY, CANCELLED);

   type Viability_Cell_Center_Type is record
      Distance     : Current_Range_Type;
      Altitude     : Current_Altitude_Type;
      Speed        : Current_Speed_Type;
   end record;

   type Viability_Distance_Center is new Positive;
   type Viability_Altitude_Center is new Positive;
   type Viability_Speed_Center is new Positive;

   type Viability_Domain_Mesh_Type is array
     (Viability_Distance_Center range <>,
      Viability_Altitude_Center range <>,
      Viability_Speed_Center range <>)
     of Viability_Cell_Center_Type;

   type Glide_Altitude_Center is new Positive;

   type Glide_Domain_Mesh_Type is array
     (Glide_Altitude_Center range <>) of Current_Altitude_Type;

   type Mission_Profile_Type is record
      Mass     : Payload_Mass_Type;
      Distance : Current_Range_Type;
      Altitude : Current_Altitude_Type;
      Speed    : Current_Speed_Type;
   end record;

   type Center_Mission_Profile_Type is record
      M : Payload_Mass_Center;
      D : Viability_Distance_Center;
      A : Viability_Altitude_Center;
      S : Viability_Speed_Center;
   end record;

   type Mission_Profile_Distance_Type is new Natural;

   type Neighbour_Mission_Profile_Type is record
      Mission_Profile : Center_Mission_Profile_Type;
      Distance        : Mission_Profile_Distance_Type;
   end record;

   type Num_Of_Neighbours is new Positive range 1 .. 16;

   type Neighbour_Mission_Profile_Array_Type is array
     (Num_Of_Neighbours range <>)
     of Neighbour_Mission_Profile_Type;

   type Neighbour_Mission_Profiles (Size : Num_Of_Neighbours) is record
     Neighbours : Neighbour_Mission_Profile_Array_Type (1 .. Size);
   end record;

   type Energy_Level_Array_Type is array
     (Num_Of_Neighbours range <>)
     of Energy_Level_Type;

   type Energy_Levels (Size : Num_Of_Neighbours) is record
     Neighbours : Energy_Level_Array_Type (1 .. Size);
   end record;

end MMS.F_PT.F_MM;
