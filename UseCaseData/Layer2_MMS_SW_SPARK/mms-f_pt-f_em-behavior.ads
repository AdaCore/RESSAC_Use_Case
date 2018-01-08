private with MMS.F_PT.F_EM.Data;

package MMS.F_PT.F_EM.Behavior with SPARK_Mode is
      
   ------------
   -- Inputs --
   ------------

   function Propulsion_Torque return Torque_Type with Global => Private_State;
   
   function Braking_Torque return Torque_Type with Global => Private_State;

   function P_Dot return Speed_Type with Global => Private_State;

   ---------------------------------------
   -- Behavioural Specification of F_EM --
   ---------------------------------------

   procedure Read_Inputs with
   --  Read values of inputs once and for all and update the current state
     Global => (In_Out => Private_State);

   procedure Write_Outputs with
   --  Compute values of outputs from the current state
     Global => (Input  => Private_State,
                Output => Output_State);

   procedure Run with
     Global => (In_Out => Private_State);
   
private
   
   function Primary_Initial_Capacity return Energy_Level_Type 
   is
     (Data.Primary_Initial_Capacity + Data.Secondary_Initial_Capacity)
   with Global => Private_State;
      
   function Propulsion_Energy return Energy_Level_Type
     with Global => Private_State;
   --  Integral [0-t] Propulsion_Torque(u) * P_Dot(u) * du
   
   function Braking_Energy return Energy_Level_Type
     with Global => Private_State;
   --  Integral [0-t] Braking_Torque(u) * P_Dot(u) * du
   
   function Dispended_Energy return Energy_Level_Type
   is 
     (Propulsion_Energy + Braking_Energy)
   with Global => Private_State;
   
   function Primary_Energy_Left return Energy_Level_Type
   is
     (Data.Primary_Initial_Capacity - Dispended_Energy)
   with Global => Private_State;
   
   function Primary_Source return Source_Type 
   is  
     (Source_Type (100 *
                   (Data.Primary_Initial_Capacity - Dispended_Energy /
                      Data.Primary_Initial_Capacity)))
   with Global => Private_State;
   
   function Secondary_Energy_Left return Energy_Level_Type
   is
     (if Primary_Source > 0 then 
         Data.Secondary_Initial_Capacity
      else
         Data.Secondary_Initial_Capacity - Dispended_Energy)
   with Global => Private_State;
   
   function Secondary_Source return Source_Type 
   is
     (if Primary_Source > 0 then 
         100
      else
         Source_Type (100 *
                      (Data.Secondary_Initial_Capacity - Dispended_Energy /
                         Data.Secondary_Initial_Capacity)))
   with Global => Private_State;
     
end MMS.F_PT.F_EM.Behavior;
