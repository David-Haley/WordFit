-- This program searches through specified directories for word fit solution
-- files. Each solution is checked to see if the solution is unique.
-- Author    : David Haley
-- Created   : 04/01/2020
-- Last Edit : 12/01/2022
-- 20220112 : Multiple Solutions report generated

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Maps;

procedure Catalogue is

   subtype Solution_File_Names is String;
   subtype Solutions is Unbounded_String;

   package Solution_Lists is new
     Ada.Containers.Indefinite_Vectors (Positive, Solution_File_Names);
   use Solution_Lists;

   package Multiple_Solution_Lists is new
     Ada.Containers.Indefinite_Ordered_Maps (Solution_File_Names, Positive);
   use Multiple_Solution_Lists;

   package Solution_Maps is new
     Ada.Containers.Ordered_Maps (Solutions, Solution_Lists.Vector);
   use Solution_Maps;

   procedure Recursive_Search (Start_At : in String;
                              Solution_List : in out Solution_Lists.Vector) is

      Search : Search_Type;
      Directory_Pattern : constant String := "*";
      Directory_Filter : constant Filter_Type :=
        (Directory => True, others => False);
      Directory_Entry : Directory_Entry_Type;
      Solution_Pattern : constant String := "*_Solution.txt";
      File_Filter : constant Filter_Type :=
        (Ordinary_File => True, others => False);

   begin -- Recursive_Search
      Start_Search (Search, Start_At, Directory_Pattern, Directory_Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Directory_Entry);
         if Simple_Name (Directory_Entry) /= "." and
           Simple_Name (Directory_Entry) /= ".." then
            Recursive_Search (Full_Name (Directory_Entry), Solution_List);
         end if; -- Simple_Name (Directory_Entry) /= "." and ...
      end loop; --  More_Entries (Search)
      End_Search (Search);
      Start_Search (Search, Start_At, Solution_Pattern, File_Filter);
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Directory_Entry);
         Append (Solution_List, Full_Name (Directory_Entry));
      end loop; -- More_Entries (Search)
      End_Search (Search);
   end Recursive_Search;

   procedure Update_Solution_Stats (Solution_File_Name : in Solution_File_Names;
                                    Solution_Map : in out Solution_Maps.Map;
                                    Multiple_Solution_List :
                                    in out Multiple_Solution_Lists.Map) is

      Solution_File : File_Type;
      Text, Solution_String : Unbounded_String;
      First : Positive;
      Last : Natural;
      Solution_Count : Natural := 0;

   begin -- Update_Solution_Stats
      Open (Solution_File, In_File, Solution_File_Name);
      while not End_Of_File (Solution_File) loop
         Get_Line (Solution_File, Text);
         Find_Token (Text, Letter_Set, Inside, First, Last);
         if Slice (Text, First, Last) = "Solution" then
            Solution_String := Null_Unbounded_String;
            Solution_Count := Solution_Count + 1;
         elsif Element (Text, 1) = '-' then
            if not Contains (Solution_Map, Solution_String) then
               Include (Solution_Map, Solution_String,
                        Solution_Lists.Empty_Vector);
            end if; -- not Contains (Solution_Map, Solution_String)
            Append (Solution_Map (Solution_String), Solution_File_Name);
         else
            Solution_String := Solution_String & Text & "|";
         end if; -- Slice (Text, First, Last) = "Solution"
      end loop; -- not End_Of_File (Solution_File)
      if Solution_Count > 1 then
         Include (Multiple_Solution_List, Solution_File_Name, Solution_Count);
      end if; -- Solution_Count > 1
      Close (Solution_File);
   end Update_Solution_Stats;

   Solution_List : Solution_Lists.Vector := Solution_Lists.Empty_Vector;
   Solution_Map : Solution_Maps.Map;
   Multiple_Solution_List : Multiple_Solution_Lists.Map;

begin -- Catalogue
   Recursive_Search (Argument (1), Solution_List);
   for S in Iterate (Solution_List) loop
      Update_Solution_Stats (Solution_List (S), Solution_Map,
                             Multiple_Solution_List);
   end loop; -- S in Iterate (Solution_List)
   Put_Line ("Duplicate Puzzle Report");
   for M in Iterate (Solution_Map) loop
      if Length (Solution_Map (M)) > 1 then
         Put_Line ("Number of repeats:" &
                     Count_Type'Image (Length (Solution_Map (M))));
         for L in Iterate (Solution_Map (M)) loop
            Put_Line (Solution_Map (M) (L));
         end loop; -- L in Iterate (Solution_Map (M))
         New_Line;
      end if; -- Length (Solution_Map (M)) > 1
   end loop; -- M in Iterate (Solution_Map)
   Put_Line ("Multiple Solution Report");
   for M in Iterate (Multiple_Solution_List) loop
      Put_Line (Key (M) & Positive'Image (Multiple_Solution_List (M)));
   end loop; -- M in Iterate (Multiple_Solution_List)
end Catalogue;
