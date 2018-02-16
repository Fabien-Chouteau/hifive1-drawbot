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
with Gcode.Motion;

with Step_Control;

with RISCV_Adacore_Gcode;
with Console;
with System; use System;

package body Gcode_Controller is

   Print_Enabled : constant Boolean := False;

   procedure Erase_All;

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

   ---------------
   -- Erase_All --
   ---------------

   procedure Erase_All is
   begin
      --  Enable motor
      Gcode_Controller.Execute ("M17");

      --  Raise pen
      Gcode.Motion.Move_Line (Ctx, (0.0, 0.0, 1.0), Feed_Rate => 100.0);

      --  Lower eraser
      Step_Control.Set_Eraser (Step_Control.Low);

      --  "Lower" pen
      Gcode.Motion.Move_Line (Ctx, (0.0, 0.0, -1.0), Feed_Rate => 100.0);

      for Y in 1 .. 12 loop
         if Y mod 2 = 0 then
            Gcode.Motion.Move_Line (Ctx, (350.0, -Float (Y * 10), -1.0), Feed_Rate => 100.0);
         else
            Gcode.Motion.Move_Line (Ctx, (50.0, -Float (Y * 10), -1.0), Feed_Rate => 100.0);
         end if;
      end loop;

      --  Right vertical cleanup
      Gcode.Motion.Move_Line (Ctx, (357.0, 0.0, -1.0), Feed_Rate => 100.0);

      --  Left vertical cleanup
      Gcode.Motion.Move_Line (Ctx, (47.0, -120.0, -1.0), Feed_Rate => 100.0);
      Gcode.Motion.Move_Line (Ctx, (47.0, 0.0, -1.0), Feed_Rate => 100.0);

      --  Go back home
      Gcode.Motion.Move_Line (Ctx, (0.0, 0.0, 1.0), Feed_Rate => 100.0);

      --  Raise eraser
      Step_Control.Set_Eraser (Step_Control.High);

      --  Disable motor
      Gcode_Controller.Execute ("M18");
   end Erase_All;

   --------------------
   -- Execution_Task --
   --------------------

   procedure Execution_Task_Body is
   begin

      loop
         Console.Print_Line ("Disable motors");
         Gcode_Controller.Execute ("M18");
         Console.Print_Line ("Wait 10 secs");
         Console.Wait_Milliseconds (10_000);

         Console.Print_Line ("Enable motors");
         Gcode_Controller.Execute ("M17");

         for FP of RISCV_Adacore_Gcode.Coords loop
            Gcode.Motion.Move_Line (Ctx, FP, Feed_Rate => 100.0);
         end loop;

         Console.Print_Line ("Disable motors");
         Gcode_Controller.Execute ("M18");

         Console.Wait_Milliseconds (1_000 * 60 * 5);

         Erase_All;
      end loop;
   end Execution_Task_Body;

end Gcode_Controller;
