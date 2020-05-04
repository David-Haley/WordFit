-- This packahe defines ann common constants and types used by the WordFit
-- suit of programs.
-- Author    : David Haley
-- Created   : 03/04/2020
-- Last Edit : 18/04/2020

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Text_IO; use Ada.Text_IO;

package Common is

   Grid_Name : constant String := "_Grid.txt";
   List_Name : constant String := "_List.txt";
   Block_Ch : constant Character := '*';
   Ruler_End_Ch : constant Character := '+';
   Open_Ch : constant Character := ' ';

   Ruler_Set : constant Character_Set := To_Set (Ruler_End_Ch & "0123456789");

   subtype Crossword_Letters is Character range 'A' .. 'Z';
   subtype Grid_Characters is Character with Static_Predicate =>
     Grid_Characters in Crossword_Letters | Block_Ch | Open_Ch;

   subtype Units is Natural range 0 .. 9;

   package Unit_IO is new Ada.Text_IO.Integer_IO (Units);

end Common;
