-- This progrem creates a blank crosswoed grid and empty word list.
-- Author    : David Haley
-- Created   : 03/04/2020
-- Last Edit : 30/12/2021
-- 20211230 : Move to next line if nothing follows a number.
-- 20200828 : Provide for progression to the next line of text if there is one
-- or more delimiters between the last word and the end of line.
-- 20200698 : Where characters not in Crossword_Set are eliminated from a
-- Suspect string resulting in a zero length string nothing is written to the
-- List_File. Previously a blank line was written.
-- 20200515 : Provided for the removal of unexpected characters, Suspect string
-- written to list and warning issued.
-- 20200511 : Ability to read mutiple files containing partial word lists into
-- into the Base_Name_List.txt file. The files to be combined must nave names
-- of the form Base_Name_List_n.txt where n is 0, 1, 2 ...

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
With Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories; use Ada.Directories;
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

   procedure Build_List is

      function Build_Part_Name (File_Number : in Natural) return String is

      begin -- Build_Part_Name
         return Argument (1) & "_List_" &
           Trim (Natural'Image (File_Number), Left) & ".txt";
      end Build_Part_Name;

      List_File, Part_File : File_Type;
      File_Number : Natural := 0;
      Text : Unbounded_String;
      Delimiter_Set : Character_Set := To_Set (' ');
      Letters : String := "LETTERS";
      Start_At, First : Positive;
      Last : Natural;
      Suspect : Unbounded_String;

   begin -- Build_List
      Create (List_File, Out_File, Argument (1) & List_Name);
      Put_Line ("Building word list file:");
      Put_Line (Name (List_File));
      while Exists (Build_Part_Name (File_Number)) loop
         Open (Part_File, In_File, Build_Part_Name (File_Number));
         Put_Line ("Reading partial list file:");
         Put_Line (Name (Part_File));
         while not End_Of_File (Part_File) loop
            Get_Line (Part_File, Text);
            Start_At := 1;
            while Start_At < Length (Text) loop
               Translate (Text, Upper_Case_Map);
               Find_Token (Text, Delimiter_Set, Start_At, Outside, First, Last);
               if Last > 0 then
                  if Ada.Strings.Fixed.Count (Slice (Text, First, Last),
                                              Crossword_Set) =
                    Last - First + 1 then
                     -- is probable word add to list
                     Put_Line (List_File, Slice (Text, First, Last));
                  elsif Ada.Strings.Fixed.Count (Slice (Text, First, Last),
                                                 Decimal_Digit_Set) =
                    Last - First + 1 then
                     -- skip number assumed to be "n LETTERS"
                     Start_At := Last + 1;
                     Find_Token (Text, Delimiter_Set, Start_At, Outside, First,
                                 Last);
                     if not Ada.Strings.Equal_Case_Insensitive (Slice (Text,
                                                                First, Last),
                                                                Letters) then
                        Put_Line ("Warning expected """ & Letters &
                                    """ and found """ &
                                    Slice (Text, First, Last) & """ in file:");
                        Put_Line (Name (Part_File));
                        Put_Line ("at Line:" &
                                    Positive_Count'Image (Line (Part_File)
                                    - 1));
                     end if; -- Equal_Case_Insensitive (Slice (Text, First ...
                     if Last = 0 then
                        Last := Length (Text);
                        -- if nothing was found ensure that parsing moves on to
                        -- next line.
                     end if; -- Last = 0
                  else
                     Suspect := Null_Unbounded_String;
                     for I in Positive range First .. Last loop
                        if Is_In (Element (Text, I), Crossword_Set) then
                           Suspect := Suspect & Element (Text, I);
                        end if; -- Is_In (Element (Text, I), Crossword_Set)
                     end loop; -- I in Positive range First .. Last
                     if Length (Suspect) > 0 then
                        Put_Line ("Warning """ & Slice (Text, First, Last) &
                                    """ changed to """ & Suspect &
                                    """, read from file:");
                        Put_Line (List_File, Suspect);
                     else
                        Put_Line ("Warning """ & Slice (Text, First, Last) &
                                    """ removed nothing written," &
                                    " read from file:");
                     end if; -- Length (Suspect) > 0
                     Put_Line (Name (Part_File));
                     Put_Line ("at Line:" &
                                 Positive_Count'Image (Line (Part_File) - 1));
                  end if; -- Ada.Strings.Fixed.Count (Slice (Text, First, ...
                  Start_At := Last + 1;
               else
                  Start_At := Length (Text);
                  -- To get here it is assumed that there was nothing other than
                  -- delimiters between Start_At and the end of line.
               end if; -- Last > 0
            end loop; -- Start_At < Length (Text)
         end loop;
         Close (Part_File);
         File_Number := File_Number + 1;
      end loop; -- Exists (Build_Part_Name (File_Number))
      Put_Line ("Words added to list" &
                  Natural'Image (Natural (Line (List_File)) - 1));
      Close (List_File);
   end Build_List;

   Height, Width : Positive;
   Grid_File : File_Type;

begin -- Blank_Files
   if Argument_Count = 3 then
      Create (Grid_File, Out_File, Argument (1) & Grid_Name);
      Width := Positive'Value (Argument (2));
      Height := Positive'Value (Argument (3));
      Put_Line ("Building skeletal grid file:");
      Put_Line (Name (Grid_File));
      Ruler (Grid_File, Width);
      for Y in Positive range 1 .. Height loop
         Unit_IO.Put (Grid_File, Y mod (Units'Last + 1), 0);
         Ada.Text_IO.Put (Grid_File, Width * Open_Ch);
         Unit_IO.Put (Grid_File, Y mod (Units'Last + 1), 0);
         New_Line (Grid_File);
      end loop; -- Y in Positive range 1 .. Height
      Ruler (Grid_File, Width);
      Close (Grid_File);
      Build_List;
   else
      Put ("Usage: Blank_Grid Base_File_Name Width Height");
   end if; -- Argument_Count = 3
end Blank_Files;
