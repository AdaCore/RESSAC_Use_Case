with MMS.F_PT.F_MM.Input; use MMS.F_PT.F_MM.Input;
with MMS.F_PT.F_MM.State; use MMS.F_PT.F_MM.State;

package body MMS.F_PT.F_MM.Behavior with SPARK_Mode is

   ----------------------------------------------
   -- Mission_Range_From_Navigation_Parameters --
   ----------------------------------------------

   function Mission_Range_From_Navigation_Parameters
      return Current_Range_Type
   is
   begin
      return Current_Range_Type (State.Navigation_Parameters.Distance) * 1852;
   end Mission_Range_From_Navigation_Parameters;

   ------------------------------------------------
   -- Operating_Point_From_Navigation_Parameters --
   ------------------------------------------------

   function Operating_Point_From_Navigation_Parameters
      return Operating_Point_Type
   is
      Operating_Point_Altitude : Current_Altitude_Type;
      Operating_Point_Speed    : Current_Speed_Type;
   begin
      --  Convert from ft to meters
      Operating_Point_Altitude := Current_Altitude_Type
        (Float (State.Navigation_Parameters.Altitude) * 0.3048);

      --  Convert from k.t to km/h
      Operating_Point_Speed := Current_Speed_Type
        (Float (State.Navigation_Parameters.Speed) * 1.853);

      return Operating_Point_Type'(Altitude => Operating_Point_Altitude,
                                   Speed    => Operating_Point_Speed);
   end Operating_Point_From_Navigation_Parameters;

   --------------------------------------------------
   -- Current_Altitude_Close_Enough_To_ref_TakeOff --
   --------------------------------------------------

   function Current_Altitude_Close_Enough_To_ref_TakeOff
      return Boolean
   is
   begin
      --  ??? Consider that we are close enough when we reach the ref
      return State.Input_Current_Altitude >= Data.Altitude_Ref_TakeOff;
   end Current_Altitude_Close_Enough_To_ref_TakeOff;

   -----------------------------------------------
   -- Current_Speed_Close_Enough_To_ref_TakeOff --
   -----------------------------------------------

   function Current_Speed_Close_Enough_To_ref_TakeOff
      return Boolean
   is
   begin
      --  ??? Consider that we are close enough when we reach the ref
      return State.Input_Current_Speed >= Data.Speed_Ref_TakeOff;
   end Current_Speed_Close_Enough_To_ref_TakeOff;

   -----------------------
   -- Emergency_Landing --
   -----------------------

   function Emergency_Landing
      return Boolean
   is
   begin
      return State.Output_Emergency_Landing;
   end Emergency_Landing;

   ------------------------------
   -- Mission_Aborted_Signaled --
   ------------------------------

   function Mission_Aborted_Signaled
      return Boolean
   is
   begin
      return State.Output_Mission_Aborted;
   end Mission_Aborted_Signaled;

   --------------------------------
   -- Mission_Cancelled_Signaled --
   --------------------------------

   function Mission_Cancelled_Signaled
      return Boolean
   is
   begin
      return State.Output_Mission_Cancelled;
   end Mission_Cancelled_Signaled;

   -----------------
   -- Read_Inputs --
   -----------------

   procedure Read_Inputs
   is
   begin
      if not (Power_State = On and then On_State = RUNNING) then
         Input_Navigation_Parameters := Navigation_Parameters;

         if Navigation_Mode_From_GS_Received then
            Input_Navigation_Mode := Navigation_Mode;
         end if;

         Input_Mode_Switch := Mode_Switch;
         Input_Navigation_Option := Navigation_Option;
         Input_Bay_Switch := Bay_Switch;
         Input_Payload_Mass := Payload_Mass;

         if USB_Key_Present then
            Input_USB_Key := USB_Key;
         end if;
      end if;

      Input_Go := Go;
      Input_On_OFF_Push_Button := On_OFF_Push_Button;
      Input_Start_Push_Button := Start_Push_Button;
      Input_Mission_Abort := Mission_Abort;
      Input_Estimated_Total_Mass := Estimated_Total_Mass;
      Input_Current_Range := Current_Range;
      Input_Current_Speed := Current_Speed;
      Input_Current_Altitude := Current_Altitude;
      Input_Current_Flight_Phase := Current_Flight_Phase;
      Input_Energy_Level := Energy_Level;
   end Read_Inputs;

   -------------------
   -- Write_Outputs --
   -------------------

   procedure Write_Outputs is
      Is_Mission_Aborted : constant Boolean :=
                             (Power_State = On and then On_State = ABORTED);
      Is_Mission_Cancelled : constant Boolean :=
                               (Power_State = ON
                                and then On_State = INIT
                                and then Init_State = CANCELLED);
   begin
      State.Output_Emergency_Landing := Is_Mission_Aborted;
      State.Output_Mission_Aborted := Is_Mission_Aborted;

      State.Output_Mission_Cancelled := Is_Mission_Cancelled;
   end Write_Outputs;

   -------------------------------------------------------
   -- Management_Of_Navigation_Modes_Options_Parameters --
   -------------------------------------------------------

   procedure Management_Of_Navigation_Modes_Options_Parameters
   is
   begin
      --  Set the navigation mode

      State.Navigation_Mode :=
        (if Navigation_Mode_From_CP = A
         or else not Navigation_Mode_From_GS_Received
         then Navigation_Mode_From_CP
         else Navigation_Mode_From_GS);

      --  Set the operating mode from parameters

      if Mission_Parameters_Defined then
         State.Operating_Mode_From_Parameters :=
           (if Navigation_Mode_From_CP = A
            or else not Operating_Mode_From_GS_Received
            then Operating_Mode_From_USB_Key
            else Operating_Mode_From_GS);
      end if;

      --  Set the operating mode

      State.Operating_Mode :=
        (if Operating_Mode_From_Parameters = ENERGY
         and then Power_State = ON
         and then On_State = RUNNING
         and then Running_State = TAKE_OFF
         then Data.Energy_Mode_Ref_TakeOff
         else Operating_Mode_From_Parameters);

      --  Set the navigation parameters

      State.Navigation_Parameters :=
        (if Navigation_Mode_From_CP = A
         or else not Navigation_Parameters_From_GS_Received
         then Navigation_Parameters_From_USB_Key
         else Navigation_Parameters_From_GS);
   end Management_Of_Navigation_Modes_Options_Parameters;

   ---------------------------------------
   -- Operating_Point_Update_Management --
   ---------------------------------------

   procedure Operating_Point_Update_Management is
   begin
      -- Set the operating point

      if On_State = RUNNING and then Running_State = TAKE_OFF then
         State.Operating_Point :=
           Operating_Point_Type'(Altitude => Data.Altitude_Ref_TakeOff,
                                 Speed    => Data.Speed_Ref_TakeOff);
      elsif not (On_State = RUNNING and then Running_State = LANDING) then
         State.Operating_Point := Operating_Point_From_Navigation_Parameters;
      end if;

      --  Set the mission range

      if Navigation_Mode = RP and then On_State = INIT Then
         State.Mission_Range := Mission_Range_From_Navigation_Parameters;
      end if;
   end Operating_Point_Update_Management;

   ---------------------
   -- Mission_Profile --
   ---------------------

   function Mission_Profile
      return Mission_Profile_Type
   is
   begin
      return State.Mission_Profile;
   end Mission_Profile;

   -------------------------------------
   -- Appropriate_Tabulating_Function --
   -------------------------------------

   function Appropriate_Tabulating_Function
      return Viability_Domain_Mesh_Type
   is
   begin
      if On_State = INIT and then Navigation_Mode = A then
         return Data.Amode_Initial_Domain_Mesh;
      elsif On_State = INIT and then Navigation_Mode = RP then
         return Data.RPmode_Initial_Domain_Mesh;
      elsif Navigation_Mode = A Then
         return Data.Amode_Cruise_Domain_Mesh;
      else
         return Data.RPmode_Cruise_Domain_Mesh;
      end if;
   end Appropriate_Tabulating_Function;

   -----------------------------
   -- Distance_With_Neighbour --
   -----------------------------

   function Distance_With_Neighbour
     (Neighbour : Mission_Profile_Type)
      return Mission_Profile_Distance_Type
   is
   begin
      return Mission_Profile_Distance_Type
        (abs (Neighbour.Distance - State.Mission_Profile.Distance));
      --  ??? not clear how the distance is computed
   end Distance_With_Neighbour;

   ------------------------
   -- Nearest_Neighbours --
   ------------------------

   function Nearest_Neighbours
      return Neighbour_Mission_Profiles
   is
      Viability_Domain_Mesh : constant Viability_Domain_Mesh_Type :=
                                Appropriate_Tabulating_Function;
      M                     : Payload_Mass_Center :=
                                F_PT.Data.Payload_Mass_Grid'First;
      D                     : Viability_Distance_Center :=
                                Viability_Domain_Mesh'First (1);
      A                     : Viability_Altitude_Center :=
                                Viability_Domain_Mesh'First (2);
      S                     : Viability_Speed_Center :=
                                Viability_Domain_Mesh'First (3);
      Neighbours_Arr        : Neighbour_Mission_Profile_Array_Type (1 .. 6);
      Last                  : Num_Of_Neighbours := 1;
   begin
      --  Search for in Payload Mass Grid
      while M < F_PT.Data.Payload_Mass_Grid'Last
        and then F_PT.Data.Payload_Mass_Grid (M) < Mission_Profile.Mass
      loop
         M := M + 1;
      end loop;

      --  Search in the Distance dimension
      while D < Viability_Domain_Mesh'Last (1)
        and then
          Viability_Domain_Mesh (D, A, S).Distance < Mission_Profile.Distance
      loop
         D := D + 1;
      end loop;

      --  Search in the Altitude dimension
      while A < Viability_Domain_Mesh'Last (2)
        and Then
          Viability_Domain_Mesh (D, A, S).Altitude < Mission_Profile.Altitude
      loop
         A := A + 1;
      end loop;

      --  Search in the Speed dimension
      while S < Viability_Domain_Mesh'Last (3)
        and then
          Viability_Domain_Mesh (D, A, S).Speed < Mission_Profile.Speed
      loop
         S := S + 1;
      end loop;

      --  Construct the list of neighbours

      declare
         Min_M : constant Payload_Mass_Center := Payload_Mass_Center'Max
           (F_PT.Data.Payload_Mass_Grid'First, M - 1);
         Min_D : constant Viability_Distance_Center :=
                   Viability_Distance_Center'Max
                     (Viability_Domain_Mesh'First (1), D);
         Min_A : constant Viability_Altitude_Center :=
                   Viability_Altitude_Center'Max
                     (Viability_Domain_Mesh'First (2), A);
         Min_S : constant Viability_Speed_Center :=
                   Viability_Speed_Center'Max
                     (Viability_Domain_Mesh'First (3), S);
         Neighbor_MP      : Neighbour_Mission_Profile_Type;
         Neighbor_Center  : Viability_Cell_Center_Type;
      begin
         for M_Idx in Min_M .. M loop
            for D_Idx in Min_D .. D loop
               for A_Idx in Min_A .. A loop
                  for S_Idx in Min_S .. S loop

                     Neighbor_MP.Mission_Profile :=
                       Center_Mission_Profile_Type'
                         (M => M_Idx,
                          D => D_Idx,
                          A => A_Idx,
                          S => S_Idx);

                     Neighbor_Center := Viability_Domain_Mesh
                       (D_Idx, A_Idx, S_Idx);

                     Neighbor_MP.Distance := Distance_With_Neighbour
                       (Mission_Profile_Type'
                          (Mass     => F_PT.Data.Payload_Mass_Grid (M_Idx),
                           Distance => Neighbor_Center.Distance,
                           Altitude => Neighbor_Center.Altitude,
                           Speed    => Neighbor_Center.Speed));

                     Neighbours_Arr (Last) := Neighbor_MP;

                     Last := Last + 1;
                  end loop;
               end loop;
            end loop;
         end loop;
      end;

      --  ??? How many neighboors should we give? See issue #31
      return Neighbours_MPs : Neighbour_Mission_Profiles (Last - 1) do
         for Idx in Neighbours_Arr'First .. Last - 1 loop
            Neighbours_MPs.Neighbours (Idx) := Neighbours_Arr (Idx);
         end loop;
      end return;
   end Nearest_Neighbours;

   -----------------------------------------
   -- Extract_Energy_Level_For_Neighbours --
   -----------------------------------------

   function Extract_Energy_Level_For_Neighbours
     (Neighbours : Neighbour_Mission_Profiles) return Energy_Levels
     with SPARK_Mode => Off
   is
      type Viability_Table_Function_Type is access
        function
          (M : Payload_Mass_Center;
           D : Viability_Distance_Center;
           A : Viability_Altitude_Center;
           S : Viability_Speed_Center) return Energy_Level_Type;

      Neighbours_Profiles : constant Neighbour_Mission_Profile_Array_Type :=
                              Neighbours.Neighbours;
      Neighbours_Energy_Levels : Energy_Levels (Neighbours_Profiles'Length);
      Viability_Table_Function : Viability_Table_Function_Type;
   begin
      --  Choose the appropriated viability table function
      if On_State = INIT and then Navigation_Mode = A then
         Viability_Table_Function := Data.Viability_Amode_Initial'Access;
      elsif On_State = INIT and then Navigation_Mode = RP then
         Viability_Table_Function := Data.Viability_RPmode_Initial'Access;
      elsif Navigation_Mode = A Then
         Viability_Table_Function := Data.Viability_Amode_Cruise'Access;
      else
         Viability_Table_Function := Data.Viability_RPmode_Cruise'Access;
      end if;

      for I in Neighbours_Energy_Levels.Neighbours'Range loop
         Neighbours_Energy_Levels.Neighbours (I) :=
           Viability_Table_Function
             (M => Neighbours_Profiles (I).Mission_Profile.M,
              D => Neighbours_Profiles (I).Mission_Profile.D,
              A => Neighbours_Profiles (I).Mission_Profile.A,
              S => Neighbours_Profiles (I).Mission_Profile.S);
      end loop;

      return Neighbours_Energy_Levels;
   end Extract_Energy_Level_For_Neighbours;

   -------------------------------
   -- Interpolated_Energy_Level --
   -------------------------------

   function Interpolated_Energy_Level
      return Energy_Level_Type
   is
      Neighbours_MPs         : constant Neighbour_Mission_Profiles :=
                                 Nearest_Neighbours;
      Neighbours             : constant Neighbour_Mission_Profile_Array_Type :=
                                 Neighbours_MPs.Neighbours;
      Neighbours_Energy_Lvls : constant Energy_Level_Array_Type :=
                                   Extract_Energy_Level_For_Neighbours
                                   (Neighbours_MPs).Neighbours;
      Int_Energy_Level       : Float := 0.0;
      K                      : Num_Of_Neighbours :=
                                 Neighbours_Energy_Lvls'First;
   begin
      --  ??? Not sure about the inverse distance interpolation formula
      for J in Neighbours'Range loop
         Int_Energy_Level := Int_Energy_Level +
           Float (Neighbours_Energy_Lvls (K)) /
             Float (Neighbours (J).Distance);
         K := K + 1;
      end loop;

      return Energy_Level_Type (Int_Energy_Level);
   end Interpolated_Energy_Level;

   -----------------------------
   -- Mission_Viability_Logic --
   -----------------------------

   procedure Mission_Viability_Logic
   is
   begin
      State.Mission_Profile := Mission_Profile_Type'
        (Mass     => Payload_Mass,
         Distance => Current_Range,
         Altitude => Current_Altitude,
         Speed    => Current_Speed);
   end Mission_Viability_Logic;

   -------------------------------------
   -- Initial_Mission_Viability_Logic --
   -------------------------------------

   procedure Initial_Mission_Viability_Logic
   is
      Cur_Energy_Level : constant Energy_Level_Type := Energy_Level;
      Int_Energy_Level : constant Energy_Level_Type :=
                           Interpolated_Energy_Level;
   begin
      if Navigation_Mode = A then
         State.Initial_Energy_Compatible_With_Mission :=
           Int_Energy_Level * 13 / 10 >= Cur_Energy_Level;
      else
         State.Initial_Energy_Compatible_With_Mission :=
           Int_Energy_Level * 11 / 10 >= Cur_Energy_Level;
      end if;
   end Initial_Mission_Viability_Logic;

   ---------------------------------------
   -- In_Flight_Mission_Viability_Logic --
   ---------------------------------------

   procedure In_Flight_Mission_Viability_Logic
   is
   begin
      --  ??? Include the safety margin here
      State.In_Flight_Energy_Compatible_With_Mission :=
        Energy_Level >= Interpolated_Energy_Level;
   end In_Flight_Mission_Viability_Logic;

   ----------------------------
   -- Current_Glide_Distance --
   ----------------------------

   function Current_Glide_Distance
      return Current_Range_Type
   is
      A           : Glide_Altitude_Center :=
                      Data.Glide_Distance_Domain_Mesh'First;
      Current_Alt : constant Current_Altitude_Type := Current_Altitude;
   begin
      while A <= Data.Glide_Distance_Domain_Mesh'Last
        and then Data.Glide_Distance_Domain_Mesh (A) < Current_Alt
      loop
         A := A + 1;
      end loop;

      return Data.Glide_Distance (A);
   end Current_Glide_Distance;

   ---------------------------------
   -- Mission_Termination_Control --
   ---------------------------------

   procedure Mission_Termination_Control
   is
   begin
      State.Descent_Over :=
        Mission_Range - Current_Range < Current_Glide_Distance;
   end Mission_Termination_Control;

   -------------------
   -- Update_States --
   -------------------

   procedure Update_States
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Update_States unimplemented");
      raise Program_Error with "Unimplemented procedure Update_States";
   end Update_States;

end MMS.F_PT.F_MM.Behavior;
