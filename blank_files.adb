-- This progrem creates a blank crosswoed grid and empty word list.
-- Author    : David Haley
-- Created   : 03/04/2020
-- Last Edit : 03/04/2020

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Common; use Common;

procedure Blank_Files is

   procedure Ruler (Grid_File : in File_Type; Width : in Positive) is

   begin -- Ruler
      Put (Grid_File, Ruler_End_Ch);
      for X in Positive range 1 .. Width loop
         Unit_IO.Put (Grid_File, X mod (Units'Last + 1), 0);
      end loop; -- X in Positive range 1 .. Width
      Put (Grid_File, Ruler_End_Ch);
      New_Line (Grid_File);
   end Ruler;

   Height, Width : Positive;
   Grid_File : File_Type;
   List_File : File_Type;

begin -- Blank_Files
   if Argument_Count = 3 then
      Create (Grid_File, Out_File, Argument (1) & Grid_Name);
      Create (List_File, Out_File, Argument (1) & List_Name);
      Width := Positive'Value (Argument (2));
      Height := Positive'Value (Argument (3));
      Ruler (Grid_File, Width);
      for Y in Positive range 1 .. Height loop
         Unit_IO.Put (Grid_File, Y mod (Units'Last + 1), 0);
         Put (Grid_File, Width * Open_Ch);
         Unit_IO.Put (Grid_File, Y mod (Units'Last + 1), 0);
         New_Line (Grid_File);
      end loop; -- Y in Positive range 1 .. Height
      Ruler (Grid_File, Width);
      Close (List_File);
      Close (Grid_File);
   else
      Put ("Usage: Blank_Grid Base_File_Name Width Height");
   end if; -- Argument_Count = 3
end Blank_Files;
