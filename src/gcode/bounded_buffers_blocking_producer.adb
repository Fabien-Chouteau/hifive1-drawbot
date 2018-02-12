------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  G N A T . B O U N D E D _ B U F F E R S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2003-2010, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with AGATE.API.Static_Semaphore;
with AGATE.API.Static_Mutex;
with AGATE.API;

package body Bounded_Buffers_Blocking_Producer is

   package Sem is new AGATE.API.Static_Semaphore (0, "sem");
   package Mutex is new AGATE.API.Static_Mutex (Priority => 1, Name => "mut");

   procedure Lock;
   procedure Unlock;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      AGATE.API.Wait_Lock (Mutex.ID);
   end Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      AGATE.API.Release (Mutex.ID);
   end Unlock;

   ------------
   -- Insert --
   ------------

   procedure Insert (Item : Element)
     -- when Not_Full
   is
   begin
      while not Not_Full loop
         AGATE.API.Wait_For_Signal (Sem.ID);
      end loop;

      Lock;

      Values (Next_In) := Item;
      Next_In := (Next_In mod Capacity) + 1;
      Count := Count + 1;
      Not_Full := Count /= Capacity;

      Unlock;
   end Insert;

   ------------
   -- Remove --
   ------------

   procedure Remove (Item : out Element) is
   begin
      Lock;

      Item := Values (Next_Out);
      Next_Out := (Next_Out mod Capacity) + 1;
      Count := Count - 1;
      Not_Full := True;
      AGATE.API.Signal (Sem.ID);

      Unlock;
   end Remove;

   -----------
   -- Empty --
   -----------

   function Empty return Boolean is
   begin
      return Count = 0;
   end Empty;

   ----------
   -- Full --
   ----------

   function Full return Boolean is
   begin
      return Count = Capacity;
   end Full;

   ------------
   -- Extent --
   ------------

   function Extent return Natural is
   begin
      return Count;
   end Extent;

end Bounded_Buffers_Blocking_Producer;
