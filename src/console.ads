with HAL;

package Console is

   procedure Print (C : Character);
   procedure Print (Str : String);
   procedure Print_Line (Str : String);

   function Time return HAL.UInt64;
   function Timer_Frequency return HAL.UInt64;
   procedure Wait_Milliseconds (Ms : Natural);
end Console;
