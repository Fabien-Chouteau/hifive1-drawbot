-------------------------------------------------------------------------------
--                                                                           --
--                                   ACNC                                    --
--                                                                           --
--         Copyright (C) 2016 Fabien Chouteau (chouteau@adacore.com)         --
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

--  with Ada.Text_IO; use Ada.Text_IO;

package body Gcode.Parameters is

   function Find (Ctx : Parameters_Set; Id : Parameter_Id) return Natural;
   function Find (Ctx : Parameters_Set; Name : String) return Natural;

   ----------
   -- Find --
   ----------

   function Find (Ctx : Parameters_Set; Id : Parameter_Id) return Natural is
   begin
      for Index in Parameter_Range'First .. Ctx.Last - 1 loop
         if Ctx.Params (Index).Name_Len = 0 then
            if Ctx.Params (Index).Id = Id then
               return Index;
            end if;
         end if;
      end loop;
      return Parameter_Range'First - 1;
   end Find;

   ----------
   -- Find --
   ----------

   function Find (Ctx : Parameters_Set; Name : String) return Natural is
   begin
      for Index in Parameter_Range'First .. Ctx.Last - 1 loop
         if Ctx.Params (Index).Name_Len /= 0 then
            if Ctx.Params (Index).Name (1 .. Ctx.Params (Index).Name_Len) = Name then
               return Index;
            end if;
         end if;
      end loop;
      return Parameter_Range'First - 1;
   end Find;

   ------------
   -- Define --
   ------------

   procedure Define (Ctx : in out Parameters_Set;
                     Id : Parameter_Id;
                     Value : Float_Value)
   is
      P : constant Natural := Find (Ctx, Id);
   begin
      if P in Parameter_Range then
--           Put_Line ("Set parameter value #" & Id'Img & " = " & Value'Img);
         Ctx.Params (P).Value := Value;
      else
         if Ctx.Last not in Parameter_Range then
            --  No error handling...
--              Put_Line ("Parameter set is full.");
            raise Program_Error;
         else
--              Put_Line ("Define parameter #" & Id'Img & " = " & Value'Img);
            Ctx.Params (Ctx.Last).Ptype := Numbered;
            Ctx.Params (Ctx.Last).Value := Value;
            Ctx.Params (Ctx.Last).Id    := Id;
            Ctx.Last := Ctx.Last + 1;
         end if;
      end if;
   end Define;

   ------------
   -- Define --
   ------------

   procedure Define (Ctx   : in out Parameters_Set;
                     Name  : String;
                     Value : Float_Value)
   is
      P : constant Natural := Find (Ctx, Name);
   begin
      if P in Parameter_Range then
--           Put_Line ("Set parameter value #<" & Name & "> = " & Value'Img);
         Ctx.Params (P).Value := Value;
      else
         if Ctx.Last not in Parameter_Range then
--              Put_Line ("Parameter set is full.");
            --  No error handling...
            raise Program_Error;
         else
--              Put_Line ("Define parameter #<" & Name & "> = " & Value'Img);
            Ctx.Params (Ctx.Last).Ptype := Numbered;
            Ctx.Params (Ctx.Last).Value := Value;
            Ctx.Params (Ctx.Last).Name_Len := Name'Length;
            Ctx.Params (Ctx.Last).Name (1 .. Name'Length) := Name;
            Ctx.Last := Ctx.Last + 1;
         end if;
      end if;
   end Define;

   -------------
   -- Defined --
   -------------

   function Defined (Ctx : Parameters_Set;
                     Id :  Parameter_Id)
                     return Boolean
   is
      P : constant Natural := Find (Ctx, Id);
   begin
      return P in Parameter_Range;
   end Defined;

   -------------
   -- Defined --
   -------------

   function Defined (Ctx  : Parameters_Set;
                     Name : String)
                     return Boolean
   is
      P : constant Natural := Find (Ctx, Name);
   begin
      return P in Parameter_Range;
   end Defined;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Ctx : Parameters_Set;
                       Id :  Parameter_Id)
                       return Float_Value
   is
      P : constant Natural := Find (Ctx, Id);
   begin
      if P in Parameter_Range then
         return Ctx.Params (P).Value;
      else
         return 0.0;
      end if;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Ctx  : Parameters_Set;
                       Name : String)
                       return Float_Value
   is
      P : constant Natural := Find (Ctx, Name);
   begin
      if P in Parameter_Range then
         return Ctx.Params (P).Value;
      else
         return 0.0;
      end if;
   end Get_Value;

   -----------
   -- Clear --
   -----------

   procedure Clear (Ctx : in out Parameters_Set) is
   begin
      for Index in Parameter_Range loop
         Ctx.Params (Index).Name_Len := 0;
      end loop;
      Ctx.Last := Parameter_Range'First;
   end Clear;


   -----------
   -- Print --
   -----------

   procedure Print (Ctx : Parameters_Set) is
   begin
--        Put_Line ("Print parameters:");
--        for Index in Parameter_Range'First .. Ctx.Last - 1 loop
--           if Ctx.Params (Index).Name /= null then
--              Put ("#" & Ctx.Params (Index).Name.all);
--           else
--              Put ("#" & Ctx.Params (Index).Id'Img);
--           end if;
--           Put_Line (" :" & Ctx.Params (Index).Value'Img);
--        end loop;
      null;
   end Print;

end Gcode.Parameters;
