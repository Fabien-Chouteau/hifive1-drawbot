-------------------------------------------------------------------------------
--                                                                           --
--                                   ACNC                                    --
--                                                                           --
--       Copyright (C) 2016-2017 Fabien Chouteau (chouteau@adacore.com)      --
--                                                                           --
--                                                                           --
--    ACNC is free software: you can redistribute it and/or modify it        --
--    under the terms of the GNU General Public License as published by      --
--    the Free Software Foundation, either version 3 of the License, or      --
--    (at your option) any later version.                                    --
--                                                                           --
--    ACNC is distributed in the hope that it will be useful, but WITHOUT    --
--    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY     --
--    or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public        --
--    License for more details.                                              --
--                                                                           --
--    You should have received a copy of the GNU General Public License      --
--    along with ACNC. If not, see <http://www.gnu.org/licenses/>.           --
--                                                                           --
-------------------------------------------------------------------------------

with Gcode.Parser;
with Gcode.Execution;
with Gcode.Error;

with RISCV_Adacore_Gcode;
with Console;
with System; use System;

package body Gcode_Controller is

--     Task_Sync : Ada.Synchronous_Task_Control.Suspension_Object;

--     task Execution_Task is
--        pragma Priority (System.Default_Priority);
--        pragma Storage_Size (40 * 1024);
--     end Execution_Task;

   Print_Enabled : constant Boolean := True;

   procedure Last_Chance_Handler
     (Msg : System.Address; Line : Integer);
   pragma Export
     (C,  Last_Chance_Handler, "__gnat_last_chance_handler");
   pragma No_Return (Last_Chance_Handler);

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler
     (Msg : System.Address; Line : Integer)
   is
      pragma Unreferenced (Line, Msg);
   begin
      Console.Print_Line ("Last chance handler");
      loop
         null;
      end loop;
   end Last_Chance_Handler;

   ---------------
   -- Initalize --
   ---------------

   procedure Initalize is
   begin
      Ctx.Output_Index := Ctx.Output_Buffer'First;
   end Initalize;

   -------------
   -- Execute --
   -------------

   procedure Execute (Str : String) is
   begin
      if not Gcode.Parser.Parse (Str, Ctx) then
         Ctx.Put_Line ("Gcode parsing failed");
         return;
      end if;

      if not Gcode.Execution.Execute (Str, Ctx) then
         Ctx.Put_Line ("Gcode execution failed");
      end if;
   exception
      when Gcode.Error.Gcode_Exception =>
         Ctx.Put_Line ("Gcode exception");
      when others =>
         Ctx.Put_Line ("other exception");
   end Execute;

   ---------
   -- Put --
   ---------

   overriding
   procedure Put (Ctx : in out CNC_Context; C : Character) is
   begin
      if Print_Enabled then
         Ctx.Output_Buffer (Ctx.Output_Index) := C;
         if Ctx.Output_Index = Ctx.Output_Buffer'Last or else C = ASCII.LF then
            Console.Print (Ctx.Output_Buffer (Ctx.Output_Buffer'First .. Ctx.Output_Index));
            Ctx.Output_Index := 1;
         else
            Ctx.Output_Index := Ctx.Output_Index + 1;
         end if;
      end if;
   end Put;

   --------------------
   -- Execution_Task --
   --------------------

   procedure Execution_Task_Body is
   begin

      loop
         for Str_Ptr of RISCV_Adacore_Gcode.Gcode loop
            Ctx.Put_Line ("Executing: '" & Str_Ptr.all & "'");
            Gcode_Controller.Execute (Str_Ptr.all);
         end loop;
         Console.Wait_Milliseconds (10_000);
      end loop;
   end Execution_Task_Body;

end Gcode_Controller;
