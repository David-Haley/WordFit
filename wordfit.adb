-- This program solves Word Fit puzzle where a grid with some blocked squares
-- is populated from a list of words. There are no clues or numbered squares.
-- Author    : David Haley
-- Created   : 03/04/2020
-- Last Edit : 04/01/2022
-- 20220104 : Solution or soloutions written to file _Solution.txt.
-- 20201117 : New command line switch c and associated
-- Enable_Fill_List_Exceptions added to allow solver to proceed to if there is a
-- missmatch between the number of items elements in the Fill_List and
-- the Word_List. Required to solve puzzles published in Women's Day.
-- 20201016 : Down scan error message corrected.
-- 20200614 : Check_Unique removed, Check_One_Direction now returns all words
-- which will fit if there are no letters to match, this makes it behave like
-- Check_Unique. A new data structure Length_List was required to initialise
-- Check_One_Direction. These changes substantially improve the performance of
-- Iterative_Place_Words. Queued_Place_Words now behaves like
-- Iterative_Place_Words when puzzles like CM_20200513 are encountered. The
-- queue is refreshed from all unused Fill_List elements. This is done once only
-- to allow puzzles with multiple solutions to be solved. The  termination
-- criteria has been tightened up to reduce unnecessary requeueing before Search
-- is invoked. New huerestics have been applied to Search to terminate descent
-- if no word can be placed in an empty Fill_List_Eleement.
-- 20200530 : Improved formatting of debug file
-- 20200529 : Corrected text message when m switch used
-- 20200515 : Corrected some spelling in comments and improved Solve exception
-- handler.
-- 20200513 : Reports Fill_Queue length if Queue stretegy does not find a
-- solution. Check_Unique used to refresh the Fill_Queue if progress stalls.
-- The Check_Unique procedure was made generic so that the same code could be
-- used in both the Iteration and Queue strategies. The Update_Procedure
-- requirements are different for the two strategies, hence the need for it to
---be generic. CM_20200513 caused the Queue strategy to fail because it got to
-- the situation where the only way to progress was to apply the Check_Unique
-- rule.
-- 20200511 : All words placed or not placed mesage written to Debug_File.
-- 20200430 : Full recursive search invoked via a command line switch.
-- 20200429 : Queue solution strategy invoked via command line switch.
-- 20200428 : Fixed issue with CM_20200409 raising a Fill_List exception due to
-- a Down word being 1 cell across. Introduced sorting of Fill_List
-- 20200427 : Corrected Negation_Check to exclude the current word under,
-- considerstion.
-- 20200425 : Debugging inprovements to identify the cause of failure on
-- CM_20200423 (multiple solutions) and CM_20200424. Assertions replaced with
-- exceptions. Verify_Fill_List identified data entry issue with CM_20200424.
-- 20200424 : Storing and display of mutiple distinct solutions;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Generic_Constrained_Array_Sort;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with NT_Console;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Common; use Common;

procedure WordFit is

   Grid_Limit_Error, Word_Limit_Error, Fill_List_Error, Update_Error,
   Unique_Error, Search_Error, Verification_Error : exception;

   type Solution_Modes is (Iterative, Map, Queued, Recursive);

   procedure Find_Limits (Grid_File : in File_Type;
                          X_Limit, Y_Limit : out Natural) is

      -- Finds size of grid and performs some validity tests.

      Text : Unbounded_String;
      Footer_Ruler : Boolean := False;
      Label : Units;
      Unused : Positive;

   begin -- Find_Limits
      Y_Limit := 0;
      while not End_Of_File (Grid_File) and not Footer_Ruler loop
         Get_Line (Grid_File, Text);
         if Y_Limit = 0 then
            if Element (Text, 1) /= Ruler_End_Ch or
              Element (Text, Length (Text)) /= Ruler_End_Ch then
               raise Grid_Limit_Error with
                 "Bad header does not start and end with '+'";
            end if; -- Element (Text, 1) /= Ruler_End_Ch or ...
            X_Limit := Length (Text) - 2;
         else
            if not Is_In (Element (Text, 1), Ruler_Set) then
               raise Grid_Limit_Error with "Line" & Natural'Image (Y_Limit) &
                 " invalid first character";
            end if; -- not Is_In (Element (Text, 1), Ruler_Set)
            if X_Limit /= Length (Text) - 2 then
               raise Grid_Limit_Error with "Line" & Natural'Image (Y_Limit) &
                 " not the same length as header";
            end if; -- not Is_In (Element (Text, 1), Ruler_Set)
         end if; -- Y_Limit = 0
         if Y_Limit > 0 and Element (Text, 1) /= Ruler_End_Ch then
            Unit_IO.Get (Slice (Text, 1, 1), Label, Unused);
            if Y_Limit mod (Units'Last + 1) /= Label then
               raise Grid_Limit_Error with "Line" & Natural'Image (Y_Limit) &
                 " start label '" & Element (Text, 1)
                 & "' does not match line number.";
            end if; -- Y_Limit mod (Units'Last + 1) /= Label
            Unit_IO.Get (Slice (Text, Length (Text), Length (Text)), Label,
                         Unused);
            if Y_Limit mod (Units'Last + 1) /= Label then
               raise Grid_Limit_Error with
                 "Line" & Natural'Image (Y_Limit) &
                 " end label '" & Element (Text, Length (Text))
                 & "' does not match line number.";
            end if; -- Y_Limit mod (Units'Last + 1) /= Label
         elsif Y_Limit > 0 and Element (Text, 1) = Ruler_End_Ch then
            if Element (Text, Length (Text)) /= Ruler_End_Ch or
              Element (Text, Length (Text)) /= Ruler_End_Ch then
               raise Grid_Limit_Error with
                 "Bad footer does not start and end with '+'";
            end if; --  Element (Text, Length (Text)) /= Ruler_End_Ch or ...
            if X_Limit /= Length (Text) - 2 then
               raise Grid_Limit_Error with "Line" & Natural'Image (Y_Limit) &
                 " not the same length as header";
            end if; -- X_Limit /= Length (Text) - 2
            Footer_Ruler := True;
         end if; -- Y_Limit > 0 and Element (Text, 1) /= Ruler_End_Ch
         if Footer_Ruler then
            Y_Limit := Y_Limit - 1;
         else
            Y_Limit := Y_Limit + 1;
         end if; -- Footer_Ruler
      end loop; -- not End_Of_File (Grid_File)
   end Find_Limits;

   procedure Find_Limits (List_File : in File_Type; Word_Count : out Positive;
                          Shortest_Word, Longest_Word : out Positive) is

      Text : Unbounded_String;
      Count : Natural := 0;

   begin -- Find_Limits
      Shortest_Word := Positive'Last;
      Longest_Word := Positive'First;
      while not End_Of_File (List_File) loop
         Get_Line (List_File, Text);
         Trim (Text, Both); -- remove leading or trailing spaces
         if Length (Text) > 0 then
            -- don't process blank lines;
            for I in Positive range 1 .. Length (Text) loop
               if not (Element (Text, I) in Crossword_Letters) then
                  raise Word_Limit_Error with "Word """ & To_String (Text) &
                    """ contains illegal character '" & Element (Text, I) & ''';
               end if; -- not (Element (Text, I) in Crossword_Letters)
            end loop; -- I in Positive range 1 .. Length (Text)
            Count := Count + 1;
            if Length (Text) < Shortest_Word then
               Shortest_Word := Length (Text);
            end if; -- Length (Text) < Shortest_Word
            if Length (Text) > Longest_Word then
               Longest_Word := Length (Text);
            end if; -- Length (Text) > Longest_Word
         end if; -- Length (Text) > 0
      end loop; -- not End_Of_File (List_File)
      if Count = 0 then
         raise Word_Limit_Error with "Empty word list";
      end if; -- Count = 0
      Word_Count := Count;
   end Find_Limits;

   procedure Solve (Grid_File, List_File : in out File_Type;
                    X_Limit, Y_Limit, X_Screen, Y_Screen : in Positive;
                    Word_Count, Shortest_Word, Longest_Word : in Positive;
                    Solution_Mode : in Solution_Modes;
                    Enable_Fill_List_Exceptions : in Boolean) is

      -- Does what it says on the can!
      -- Note Enable_Fill_List_Exceptions is used as a global switch and is not
      -- passed as aparameter.

      Word_Time : Duration := 0.1;
      Pass_Time : Duration := 1.0;
      Solution_Time : Duration := 10.0;
      Debug_Line : Positive_Count := 80;

      package Screen is new NT_Console (X_Screen, Y_Screen);
      use Screen;

      subtype X_Coordinates is Positive range 1 .. X_Limit;
      subtype Y_Coordinates is Positive range 1 .. Y_Limit;

      subtype Word_Indices is Positive range 1 .. Word_Count;
      type Word_List_Element is record
         Word : Unbounded_String;
         Used : Boolean := False;
      end record; -- Word_List_Element
      type Word_Lists is array (Word_Indices) of Word_List_Element;

      subtype Word_Lengths is Positive range Shortest_Word .. Longest_Word;
      type Directions is (Across, Down);

      type Fill_List_Indices is new Positive;
      type Fill_List_Elements is record
         X : X_Coordinates;
         Y : Y_Coordinates;
         Direction : Directions;
         Word_Length : Word_Lengths;
         Used : Boolean := False;
         Word_Index : Word_Indices := 1;
      end record; -- Fill_List_Elements

      package Fill_Lists is new
        Ada.Containers.Vectors (Fill_List_Indices, Fill_List_Elements);
      use Fill_Lists;

      type Cross_Arrays is array (Directions) of Fill_Lists.Extended_Index;
      type Grid_Elements is record
         Ch : Grid_Characters;
         Cross_Array : Cross_Arrays :=
           (Fill_Lists.No_Index, Fill_Lists.No_Index);
      end record; -- Grid_Elements
      type Grids is array (X_Coordinates, Y_Coordinates) of Grid_Elements;

      subtype Letter_Positions is Positive range 1 .. Word_Lengths'Last;
      type Word_Keys is record
         Word_Length : Word_Lengths;
         Letter : Crossword_Letters;
         Letter_Position : Letter_Positions;
      end record; -- Word_Keys

      function "<" (Left, Right : Word_Keys) return Boolean is

      begin -- "<"
         if Left.Word_Length < Right.Word_Length then
            return True;
         elsif Left.Word_Length = Right.Word_Length then
            if Left.Letter < Right.Letter then
               return True;
            elsif Left.Letter = Right.Letter then
               return Left.Letter_Position < Right.Letter_Position;
            else
               return False;
            end if; -- Left.Letter < Right.Letter
         else
            return False;
         end if; -- Left.Word_Length < Right.Word_Length
      end "<";

      package Word_Sets is new
        Ada.Containers.Ordered_Sets (Word_Indices);
      use  Word_Sets;

      package Word_Maps is new
        Ada.Containers.Ordered_Maps (Word_Keys,  Word_Sets.Set);
      use Word_Maps;

      type Length_Lists is array (Word_Lengths) of Word_Sets.Set;

      subtype Solution_Indices is Positive range
        1 .. X_Coordinates'Last * Y_Coordinates'Last;

      subtype Solution_Strings is String (Solution_Indices);

      package Solution_Sets is new
        Ada.Containers.Ordered_Sets (Solution_Strings);
      use Solution_Sets;

      subtype Update_Types is Character with
        Static_Predicate => Update_Types in 'n' | 'o';

      package Nat_IO is new Ada.Text_IO.Integer_IO (Natural);
      package Bool_IO is new Ada.Text_IO.Enumeration_IO (Boolean);
      package X_IO is new Ada.Text_IO.Integer_IO (X_Coordinates);
      package Y_IO is new Ada.Text_IO.Integer_IO (Y_Coordinates);
      package WL_IO is new Ada.Text_IO.Integer_IO (Word_Lengths);
      package LP_IO is new Ada.Text_IO.Integer_IO (Letter_Positions);
      package FL_IO is new Ada.Text_IO.Integer_IO (Fill_List_Indices);
      package WI_IO is new Ada.Text_IO.Integer_IO (Word_Indices);
      package Dir_IO is new Ada.Text_IO.Enumeration_IO (Directions);

      Debug_File, Solution_File : File_Type;

      procedure Put (Grid : in Grids;
                     X : in X_Coordinates;
                     Y : in Y_Coordinates) is
      begin -- Put
         Goto_XY (X - 1, Y - 1);
         Put (Grid (X, Y).Ch);
      end Put;

      procedure Read_Grid (Grid_File : in out File_Type;
                           Grid : out Grids) is

         Text : Unbounded_String;

      begin -- Read_Grid
         Put_Line (Debug_File, "Grid File: " & Name (Grid_File));
         Reset (Grid_File);
         Skip_Line (Grid_File); -- discard Header
         for Y in Y_Coordinates loop
            Get_line (Grid_File, Text);
            Trim (Text, Ruler_Set, Ruler_Set);
            for X in X_Coordinates loop
               Grid (X, Y).Ch := Element (Text, X);
               Put (Grid, X, Y);
            end loop; -- X in X_Coordinates
         end loop; -- Y in Y_Coordinates
         -- Footer is not read;
      end Read_Grid;

      procedure Read_Word_List (List_File : in out File_Type;
                                Word_List : out Word_Lists) is

         function "<" (Left, Right : Word_List_Element) return Boolean is

            -- Shorter words are treated as "smaller" so thet the list is sorted
            -- shortest to longest and then by collation orded of Strings.

         begin -- "<"
            if Length (Left.Word) < Length (Right.Word) then
               return True;
            elsif Length (Left.Word) = Length (Right.Word) and then
              Left.Word < Right.Word then
               return True;
            else
               return False;
            end if; -- Length (Left.Word) < Length (Right.Word)
         end "<";

         procedure Sort is new Ada.Containers.Generic_Constrained_Array_Sort
           (Element_Type => Word_List_Element,
            Index_Type => Word_Indices,
            Array_Type => Word_Lists);

         I : Word_Indices := 1;
         Text : Unbounded_String;

      begin -- Read_Word_List
         Put_Line (Debug_File, "Word List: " & Name (List_File));
         Reset (List_File);
         while not End_Of_File (List_File) loop
            Get_Line (List_File, Text);
            Trim (Text, Both); -- remove leading or trailing spaces
            if Length (Text) > 0 then
               Word_List (I).Word := Text;
               if I < Word_Indices'Last then
                  I := I + 1;
               end if; -- I < Word_Indices'Last
            end if; -- Length (Text) > 0
         end loop; -- not End_Of_File (List_File)
         Sort (Word_List);
      end Read_Word_List;

      procedure Build_Length_List (Word_List : in Word_Lists;
                                   Length_List : out Length_Lists) is

         -- Builds an array of Word_Sets where each array element is the set of
         -- words of length equal to the array index.

      begin -- Build_Length_List
         Length_List := (others => Word_Sets.Empty_Set);
         for I in Word_Indices loop
            Include (Length_List (Length (Word_List (I).Word)), I);
         end loop; -- I in Word_Indices
      end Build_Length_List;

      procedure Put (Output_File : in File_Type;
                     Grid : in Grids) is

      begin -- Put
         for Y in Y_Coordinates loop
            for X in X_Coordinates loop
               Put (Output_File, Grid (X, Y).Ch);
            end loop; -- X in X_Coordinates
            New_Line (Output_File);
         end loop; -- Y in Y_Coordinates
         Put_Line (Output_File, X_Coordinates'Last * '-');
      end Put;

      procedure Put_Map (Word_List : in Word_Lists;
                         Word_Map : in  Word_Maps.Map) is

      begin -- Put_Map
         Put_Line (Debug_File, "Word_Map Structure Report");
         Put_Line (Debug_File, "      P");
         Put_Line (Debug_File, "      o");
         Put_Line (Debug_File, " L L  s");
         Put_Line (Debug_File, " e e  i");
         Put_Line (Debug_File, " n t  t");
         Put_Line (Debug_File, " g t  i");
         Put_Line (Debug_File, " t e  o");
         Put_Line (Debug_File, " h r  n Word Set");
         for I in Iterate (Word_Map) loop
            WL_IO.Put (Debug_File, Key (I).Word_Length, 2);
            Put (Debug_File, ' ' & Key (I).Letter);
            LP_IO.Put (Debug_File, Key (I).Letter_Position, 3);
            for E in Iterate (Word_Map (I)) loop
               if Col (Debug_File) + Positive_Count (Key (I).Word_Length)
                 >= Debug_Line then
                  New_Line (Debug_File);
                  Put (Debug_File, 7 * ' ');
               end if; --  Col (Debug_File) ...
               Put (Debug_File, " " & Word_List (Element (E)).Word);
            end loop; -- E in Iterate (Word_Map (I))
            New_Line (Debug_File);
         end loop; -- I in Iterate (Word_Map) loop
      end; -- Put_Map

      procedure Debug_Data_Structures (Grid : in Grids;
                                       Word_List : in Word_Lists;
                                       Length_List : in Length_Lists;
                                       Fill_List : in Fill_Lists.Vector;
                                       Word_Map : in  Word_Maps.Map) is
         F :  Fill_List_Indices;

      begin -- Debug_Data_Structures
         Put_Line (Debug_File, "Grid Structure Report");
         Put_Line (Debug_File, "Coord     Fill List");
         Put_Line (Debug_File, " X  Y Chr Accr Down");
         for Y in Y_Coordinates loop
            for X in X_Coordinates loop
               if Grid (X, Y).Ch /= Block_Ch then
                  X_IO.Put (Debug_File, X, 2);
                  Y_IO.Put (Debug_File, Y, 3);
                  Put (Debug_File, " '" & Grid (X, Y).Ch & ''');
                  for D in Directions loop
                     if Grid (X, Y).Cross_Array (D) = Fill_Lists.No_Index then
                        Put (Debug_File, "   --");
                     else
                        F := Grid (X, Y).Cross_Array (D);
                        FL_IO.Put (Debug_File, Grid (X, Y).Cross_Array (D), 5);
                     end if; --  Grid (X, Y).Cross_Array (D) = ...
                  end loop; -- D in Directions
                  New_Line (Debug_File);
               end if; -- Grid (X, Y).Ch /= Block_Ch
            end loop; -- X in X_Coordinates
         end loop; -- Y in Y_Coordinates
         Put_Line (Debug_File, "Word_List Structure Report");
         Put_Line (Debug_File, "Index Placed Word");
         for I in Word_Indices loop
            WI_IO.Put (Debug_File, I, 5);
            Put (Debug_File, ' ');
            Bool_IO.Put (Debug_File, Word_List (I).Used, 7, Lower_Case);
            Put (Debug_File, Word_List (I).Word);
            New_Line (Debug_File);
         end loop; -- I in Word_Indices
         Put_Line (Debug_File, "Length_List Structure Report");
         for WL in Word_Lengths loop
            if Length_List (WL) /= Word_Sets.Empty_Set then
               WL_IO.Put (Debug_File, WL, 2);
               Put_Line (Debug_File, " Letter Words");
               for S in Iterate (Length_List (WL)) loop
                  if Col (Debug_File) + Positive_Count (WL) >= Debug_Line then
                     New_Line (Debug_File);
                  end if; -- Col (Debug_File) + Positive_Count (WL) >= ...
                  Put (Debug_File, ' ' & Word_List (Element (S)).Word);
               end loop; -- S in Iterate (Length_List (WL))
               New_Line (Debug_File);
            end if; -- Length_List (WL) /= Word_Sets.Empty_Set
         end loop; -- WL in Word_Lengths
         Put_Line (Debug_File, "Fill_List Structure Report");
         Put_Line (Debug_File, "Fill  Coord Direct");
         Put_Line (Debug_File, "Index  X  Y ion    Word");
         for I in Iterate (Fill_List) loop
            FL_IO.Put (Debug_File, To_Index (I), 5);
            X_IO.Put (Debug_File, Fill_List (I).X, 3);
            Y_IO.Put (Debug_File, Fill_List (I).Y, 3);
            Put (Debug_File, ' ');
            Dir_IO.Put (Debug_File, Fill_List (I).Direction, 7, Lower_Case);
            if Fill_List (I).Used then
               Put_Line (Debug_File, Word_List (Fill_List (I).Word_Index).Word);
            else
               Put_Line (Debug_File, '"' & Fill_List (I).Word_Length * ' ' &
                        '"');
            end if; -- Fill_List (I).Used
         end loop; -- I in Iterate (Fill_List (W, D))
         Put_Map (Word_List, Word_Map);
      end Debug_Data_Structures;

      procedure Debug_Word_Set (Set : in Word_Sets.Set;
                                Word_List : in Word_Lists;
                                Label : in String := "") is

      begin -- Debug_Word_Set
         Put (Debug_File, Label);
         for S in Iterate (Set) loop
            Put (Debug_File, ' ' & Word_List (Element (S)).Word);
         end loop; -- S in Iterate (Set)
         New_Line (Debug_File);
      end Debug_Word_Set;

      procedure Verify_Fill_List (Word_List : in Word_Lists;
                                  Fill_List : in Fill_Lists.Vector) is

         -- Raises an exception when Enable_Fill_List_Exceptions is true and
         -- the the available spaces for words of a specific length does not
         -- match the number of words of that length.

         type Check_Element is record
            Fill_Count, Word_Count : Natural := 0;
         end record; -- Check_Element

         Check_Array : array (Word_Lengths, Boolean) of Check_Element;

      begin -- Verify_Fill_List
         Put_Line (Debug_File, "Verification of Fill_List");
         if not Enable_Fill_List_Exceptions then
            Put_Line (Debug_File, "No mismatch exceptions will be raise");
         end if; -- not Enable_Fill_List_Exceptions
         for F in Iterate (Fill_List) loop
            Check_Array (Fill_List (F).Word_Length,
                         Fill_List (F).Used).Fill_Count :=
              Check_Array (Fill_List (F).Word_Length,
                           Fill_List (F).Used).Fill_Count + 1;
         end loop; -- F in Iterate (Fill_List)
         for I in Word_Indices loop
            Check_Array (Length (Word_List (I).Word),
                         Word_List (I).Used).Word_Count :=
              Check_Array (Length (Word_List (I).Word),
                           Word_List (I).Used).Word_Count + 1;
         end loop; -- I in Word_Indices
         Put_Line (Debug_File, "       Not Placed   Placed");
         Put_Line (Debug_File, " Word  Fill  Word Fill  Word");
         Put_Line (Debug_File, "Length List  List List  List");
         for W in Word_Lengths loop
            WL_IO.Put (Debug_File, W, 6);
            Nat_IO.Put (Debug_File, Check_Array (W, False).Fill_Count, 5);
            Nat_IO.Put (Debug_File, Check_Array (W, False).Word_Count, 6);
            Nat_IO.Put (Debug_File, Check_Array (W, True).Fill_Count, 5);
            Nat_IO.Put (Debug_File, Check_Array (W, True).Word_Count, 6);
            New_Line (Debug_File);
            if Enable_Fill_List_Exceptions then
               if Check_Array (W, False).Fill_Count /=
                 Check_Array (W, False).Word_Count then
                  raise Verification_Error with "For word length" &
                    Word_Lengths'Image (W) & " there are" &
                    Natural'Image (Check_Array (W, False).Fill_Count) &
                    " spaces to fill" &
                    Natural'Image (Check_Array (W, False).Word_Count) &
                    " words available";
               end if; -- Check_Array (W, False).Fill_Count /= ...
               if Check_Array (W, True).Fill_Count /=
                 Check_Array (W, True).Word_Count then
                  raise Verification_Error with "For word length" &
                    Word_Lengths'Image (W) & " there are" &
                    Natural'Image (Check_Array (W, False).Fill_Count) &
                    " spaces filled" &
                    Natural'Image (Check_Array (W, False).Word_Count) &
                    " words used";
               end if; -- Check_Array (W, True).Fill_Count /= /= ...
            end if; -- Enable_Fill_List_Exceptions
         end loop; -- W in Word_Lengths
         Put_Line (Debug_File, "Verification of Fill_List Complete");
      end Verify_Fill_List;

      procedure Build_Fill_List (Grid : in Grids;
                                 Word_List : in out Word_Lists;
                                 Fill_List : out Fill_Lists.Vector) is

         -- Creates a list of the the spaces in which words may be placed. The
         -- spaces of starting word or words are marked used and where they
         -- occur in the word list is recorded. The starting word or words are
         -- marked as used in the word list.

         Origin_X : X_Coordinates := X_Coordinates'First;
         Origin_Y : Y_Coordinates := Y_Coordinates'First;
         Down_Priority : Boolean := True;

         procedure Fill_List_Sort (Fill_List : in out Fill_Lists.Vector;
                                   Origin_X : in X_Coordinates;
                                   Origin_Y : in Y_Coordinates) is

            function "<" (Left, Right : Fill_List_Elements) return Boolean is

               -- Sorts on "Manhallan distance from (Origin_X, Origin_Y) and
               -- then Direction.

            begin --"<"
               if abs (Left.X - Origin_X) + abs (Left.Y - Origin_Y) <
               abs (Right.X - Origin_X) + abs (Right.Y - Origin_Y) then
                  return True;
               elsif  abs (Left.X - Origin_X) + abs (Left.Y - Origin_Y) =
               abs (Right.X - Origin_X) + abs (Right.Y - Origin_Y) then
                  return (Left.Direction < Right.Direction) xor Down_Priority;
               else
                  return False;
               end if; -- abs (Left.X - Origin_X) + abs (Left.Y - Origin_Y) ...
            end "<";

            package Fill_List_Sorts is new Fill_Lists.Generic_Sorting;
            use Fill_List_Sorts;

         begin -- Fill_List_Sort
            Fill_List_Sorts.Sort (Fill_List);
         end Fill_List_Sort;

         List_Element : Fill_List_Elements;
         Run : Natural;
         Content : Unbounded_String;

      begin -- Build_Fill_List
         Fill_List := Fill_Lists.Empty_Vector;
         List_Element.Y := 1;
         List_Element.Direction := Across;
         loop -- search Across
            List_Element.X := 1;
            loop -- scan row
               Run := 0;
               List_Element.Used := True;
               List_Element.Word_Index := 1;
               Content := Null_Unbounded_String;
               -- Determine length if Fill_List element
               while List_Element.X + Run <= X_Coordinates'Last and then
                 Grid (List_Element.X + Run,
                       List_Element.Y).Ch /= Block_Ch loop
                  List_Element.Used := List_Element.Used and
                    Grid (List_Element.X + Run, List_Element.Y).Ch in
                    Crossword_Letters;
                  Append (Content,
                          Grid (List_Element.X + Run, List_Element.Y).Ch);
                  Run := Run + 1;
               end loop; -- List_Element.X + Run <= X_Coordinates'Last ...
               if Run in Word_Lengths then
                  -- Eliminates case of there being just one cell across
                  List_Element.Word_Length := Run;
                  if List_Element.Used then
                     Origin_X := List_Element.X;
                     Origin_Y := List_Element.Y;
                     Down_Priority := True;
                     for I in Word_Indices loop
                        if Word_List (I).Word = Content then
                           List_Element.Word_Index := I;
                           Word_List (I).Used := True;
                        end if; -- Word_List (I).Word = Content
                     end loop; -- I in Word_Indices
                  end if; -- List_Element.Used
                  Append (Fill_List, List_Element);
               elsif Run > 1 then
                  raise Fill_List_Error with "At (" &
                    X_Coordinates'Image (List_Element.X) & ',' &
                    Y_Coordinates'Image (List_Element.Y) &
                    ") available Across space" & Natural'Image (Run) &
                    " does not match Word_Lengths range" &
                    Word_Lengths'Image (Word_Lengths'First) & " .." &
                    Word_Lengths'Image (Word_Lengths'Last);
               end if; -- Run in Word_Lengths
               if List_Element.X + Run + 1 < X_Coordinates'Last then
                  List_Element.X := List_Element.X + Run + 1;
               else
                  exit;
               end if; -- List_Element.X + Run + 1 < X_Coordinates'Last
            end loop; -- scan row
            exit when List_Element.Y >= Y_Coordinates'Last;
            List_Element.Y := List_Element.Y + 1;
         end loop; -- search Accross
         List_Element.X := 1;
         List_Element.Direction := Down;
         loop -- search Down
            List_Element.Y := 1;
            loop -- scan column
               Run := 0;
               List_Element.Used := True;
               List_Element.Word_Index := 1;
               Content := Null_Unbounded_String;
               -- Determine length if Fill_List element
               while List_Element.Y + Run <= Y_Coordinates'Last and then
                 Grid (List_Element.X,
                       List_Element.Y + Run).Ch /= Block_Ch loop
                  List_Element.Used := List_Element.Used and
                    Grid (List_Element.X, List_Element.Y + Run).Ch in
                    Crossword_Letters;
                  Append (Content,
                          Grid (List_Element.X, List_Element.Y + Run).Ch);
                  Run := Run + 1;
               end loop; -- List_Element.Y + Run <= Y_Coordinates'Last ...
               if Run in Word_Lengths then
                  -- Eliminates case of there being just one cell down
                  List_Element.Word_Length := Run;
                  if List_Element.Used then
                     Origin_X := List_Element.X;
                     Origin_Y := List_Element.Y;
                     Down_Priority := False;
                     for I in Word_Indices loop
                        if Word_List (I).Word = Content then
                           List_Element.Word_Index := I;
                           Word_List (I).Used := True;
                        end if; -- Word_List (I).Word = Content
                     end loop; -- I in Word_Indices
                  end if; -- List_Element.Used
                  Append (Fill_List, List_Element);
               elsif Run > 1 then
                  raise Fill_List_Error with "At (" &
                    X_Coordinates'Image (List_Element.X) & ',' &
                    Y_Coordinates'Image (List_Element.Y) &
                    ") available Down space" & Natural'Image (Run) &
                    " does not match Word_Lengths range" &
                    Word_Lengths'Image (Word_Lengths'First) & " .." &
                    Word_Lengths'Image (Word_Lengths'Last);
               end if; -- Run in Word_Lengths
               if List_Element.Y + Run + 1 < Y_Coordinates'Last then
                  List_Element.Y := List_Element.Y + Run + 1;
               else
                  exit;
               end if; -- List_Element.Y + Run + 1 < Y_Coordinates'Last
            end loop; -- scan column
            exit when List_Element.X >= X_Coordinates'Last;
            List_Element.X := List_Element.X + 1;
         end loop; -- search Down
         Fill_List_Sort (Fill_List, Origin_X, Origin_Y);
         Verify_Fill_List (Word_List, Fill_List);
      end Build_Fill_List;

      procedure Build_Cross_Reference (Grid : in out Grids;
                                       Fill_List : in Fill_Lists.Vector) is

         -- Stores the fill list entries that contain each grid element. A Grid
         -- element can be in at most one Across word and one Down word. By
         -- default the entries are set to No_Index

         X : X_Coordinates;
         Y : Y_Coordinates;

      begin -- Build_Cross_Reference
         for F in Iterate (Fill_List) loop
            X := Fill_List (F).X;
            Y := Fill_List (F).Y;
            for P in Letter_Positions range 1 .. Fill_List (F).Word_Length loop
               if Fill_List (F).Direction = Across then
                  X := Fill_List (F).X + P - 1;
               else
                  Y := Fill_List (F).Y + P - 1;
               end if; -- Fill_List (F).Direction = Across
               Grid (X, Y).Cross_Array (Fill_List (F).Direction) :=
                 To_Index (F);
            end loop; -- P in Letter_Positions range 1 ...
         end loop; -- F in Iterate (Fill_List)
      end Build_Cross_Reference;

      procedure Build_Word_Map (Word_List : in Word_Lists;
                                Word_Map : out Word_Maps.Map) is

         -- The word map is built to allow the direct look up of a word of
         -- specific length containg a specific letter in a specific position
         -- within the word. A word will appear in as many entries as it has
         -- letters. A set is stored containing the Word_Indices for all the
         -- words satisfying the length, letter and position criteria.

         Key : Word_Keys;

      begin -- Build_Word_Map
         Clear (Word_Map);
         for I in Word_Indices loop
            Key.Word_Length := Length (Word_List (I).Word);
            for P in Letter_Positions range 1 .. Key.Word_Length loop
               Key.Letter_Position := P;
               Key.Letter := Element (Word_List (I).Word, P);
               if not Contains (Word_Map, Key) then
                  Include (Word_Map, Key, Word_Sets.Empty_Set);
               end if; -- not Contains (Word_Map, Key)
               Include (Word_Map (Key), I);
            end loop; -- P in Letter_Positions range 1 .. Key.Word_Length
         end loop; -- I in Word_Indices
      end Build_Word_Map;

      procedure Check_One_Direction (Grid : in Grids;
                                     Word_List : in Word_Lists;
                                     Length_List : Length_Lists;
                                     Fill_List : in Fill_Lists.Vector;
                                     Word_Map : in Word_Maps.Map;
                                     F : in Fill_List_Indices;
                                     Placement_Set : in out Word_Sets.Set) is

         -- Finds the set of words which will fit a single Fill_List element,
         -- that is, looks Across or Down but not both. If there are no kown
         -- letters for that entry then the set will contain all words of the
         -- matching length. Where there known letters only the set of words
         -- containing those letters are returned

         Word_Key : Word_Keys;
         X : X_Coordinates := Fill_List (F).X;
         Y : Y_Coordinates := Fill_List (F).Y;
         Trial_Set : Word_Sets.Set := Word_Sets.Empty_Set;

      begin -- Check_One_Direction
         Placement_Set := Length_List (Fill_List (F).Word_Length);
         Word_Key.Word_Length := Fill_List (F).Word_Length;
         for P in Letter_Positions range 1 .. Fill_List (F).Word_Length loop
            Clear (Trial_Set);
            Word_Key.Letter_Position := P;
            if Fill_List (F).Direction = Across then
               X := Fill_List (F).X + P - 1;
            else
               Y := Fill_List (F).Y + P - 1;
            end if; -- Fill_List (F).Direction = Across
            if Grid (X, Y).Ch in Crossword_Letters then
               Word_Key.Letter := Grid (X, Y).Ch;
               if Contains (Word_Map, Word_Key) then
                  For S in Iterate (Word_Map (Word_Key)) loop
                     if not Word_List (Element (S)).Used then
                        Include (Trial_Set, Element (S));
                     end if; -- not Word_List (Element (S)).Used
                  end loop; -- S in Iterate (Word_Map (Word_Key))
               end if; -- Contains (Word_Map, Word_Key)
               Intersection (Placement_Set, Trial_Set);
            end if; -- Grid (X, Y).Ch in Crossword_Letters
         end loop; -- P in Letter_Positions range 1 ...
      end Check_One_Direction;

      procedure Negation_Check (Grid : in Grids;
                                Word_List : in Word_Lists;
                                Length_List : Length_Lists;
                                Fill_List : in Fill_Lists.Vector;
                                Word_Map : in Word_Maps.Map;
                                Fp : in Fill_List_Indices;
                                Prime_Word_Set : in out Word_Sets.Set) is

         -- Removes elements from the prime word set if no possible Cross word
         -- has the same letter in common at their intersection.

         function Count_Options (Grid_In : in Grids;
                                 Word_List : in Word_Lists;
                                 Length_List : Length_Lists;
                                 Fill_List : in Fill_Lists.Vector;
                                 Word_Map : in Word_Maps.Map;
                                 Xp : in X_Coordinates;
                                 Yp : in Y_Coordinates;
                                 Pp : in Letter_Positions;
                                 Dc : in Directions;
                                 Word_Index : in Word_Indices)
                                 return Count_Type is

            -- Counts the number of words satisfying the condition of having
            -- the same letter at the intersection of the Prime and Cross
            -- words. It is possible that the current cross word has already
            -- been found, in which case 1 is returned.

            Grid : Grids := Grid_In;
            Cross_Word_Set : Word_Sets.Set;

         begin -- Count_Options
            if Fill_List (Grid (Xp, Yp).Cross_Array (Dc)).Used then
               -- This space has already been filled in
               return 1;
            end if; -- Fill_List (Grid (Xp, Yp).Cross_Array (Dc)).Used
            Grid (Xp, Yp).Ch := Element (Word_List (Word_Index).Word, Pp);
            -- Set the letter in the appropriate position for the word being
            -- currently being considered.
            Check_One_Direction (Grid, Word_List, Length_List, Fill_List,
                                 Word_Map, Grid (Xp, Yp).Cross_Array (Dc),
                                 Cross_Word_Set);
            -- Exclude the prime word, cannot be crossed with itself. that
            -- is, only one instance of a word is allowed!
            Exclude (Cross_Word_Set, Word_Index);
            return Length (Cross_Word_Set);
         end Count_Options;

         Xp : X_Coordinates := Fill_List (Fp).X;
         Yp : Y_Coordinates := Fill_List (Fp).Y;
         Dc : Directions;
         Fc : Fill_Lists.Extended_Index;
         Exclusion_Set : Word_Sets.Set := Word_Sets.Empty_Set;

      begin -- Negation_Check
         -- Calculate cross word direction
         if Fill_List (Fp).Direction = Across then
            Dc := Down;
         else
            Dc := Across;
         end if; -- Fill_List (Fp).Direction = Across
         for Pp in Letter_Positions
         range 1 .. Fill_List (Fp).Word_length loop
            -- Calculate the coordinates of the intersection letter
            if Fill_List (Fp).Direction = Across then
               Xp := Fill_List (Fp).X + Pp - 1;
            else
               Yp := Fill_List (Fp).Y + Pp - 1;
            end if; --  Fill_List (Fp).Direction = Across
            -- Obtain the fill list entry for the current potential
            -- intersection Cross word.
            Fc := Grid (Xp, Yp).Cross_Array (Dc);
            if Fc /= Fill_Lists.No_Index then
               -- A Cross word exists
               for S In Iterate (Prime_Word_Set) loop
                  if Count_Options (Grid, Word_List, Length_List, Fill_List,
                                    Word_Map, Xp, Yp, Pp, Dc, Element (S))
                    = 0 then
                     Include (Exclusion_Set, Element (S));
                  end if; -- Count_Options ...
               end loop; -- S In Iterate (Prime_Word_Set)
            end if; -- Fc /= Fill_Lists.No_Index
         end loop; -- P in Letter_Positions range ...
         Symmetric_Difference (Prime_Word_Set, Exclusion_Set);
      end Negation_Check;

      function Solution_Index (X : in X_Coordinates; Y : in Y_Coordinates)
                               return Solution_Indices is

      begin -- Solution_Index
         return X + X_Coordinates'Last * (Y - 1);
      end Solution_Index;

      procedure Save_Solution (Grid : in Grids;
                               Solution_Set : in out Solution_Sets.Set) is

         Solution_String : Solution_Strings;

      begin --Save_Solution
         for Y in Y_Coordinates loop
            for X in X_Coordinates loop
               Solution_String (Solution_Index (X, Y)) := Grid (X, Y).Ch;
            end loop; -- X in X_Coordinates
         end loop; -- Y in Y_Coordinates
         Include (Solution_Set, Solution_String);
      end Save_Solution;

      procedure Display (Solution_Set : in Solution_Sets.Set) is

         Solution_Count : Positive := 1;
         Solution_String : Solution_Strings;

      begin -- Display
         for S in Iterate (Solution_Set) loop
            Solution_String := Element (S);
            Clear_Screen;
            for Y in Y_Coordinates loop
               Goto_XY (X_Pos'First, Y - 1);
               for X in X_Coordinates loop
                  Put (Solution_String (Solution_Index (X, Y)));
               end loop; -- X in X_Coordinates
            end loop; -- Y in Y_Coordinates
            Goto_XY (X_Pos'First, Y_Coordinates'Last);
            Put_Line ("Solution" & Positive'Image (Solution_Count) & " of" &
                        Count_Type'Image (Length (Solution_Set)) &
                        " found by Search");
            Solution_Count := Solution_Count + 1;
            if S /= Last (Solution_Set) then
               delay Solution_Time;
            end if; -- S /= Last (Solution_Set)
         end loop; -- S in Iterate (Solution_Set)
      end Display;

      procedure Put (Output_File : in File_Type;
                     Solution_Set : in Solution_Sets.Set) is

         Solution_Count : Positive := 1;
         Solution_String : Solution_Strings;

      begin -- Put
         for S in Iterate (Solution_Set) loop
            Put_Line (Output_File,
                      "Solution" & Positive'Image (Solution_Count) & " of" &
                        Count_Type'Image (Length (Solution_Set)) &
                        " found by Search");
            Solution_String := Element (S);
            for Y in Y_Coordinates loop
               for X in X_Coordinates loop
                  Put (Output_File, Solution_String (Solution_Index (X, Y)));
               end loop; -- X in X_Coordinates
               New_Line (Output_File);
            end loop; -- Y in Y_Coordinates
            Solution_Count := Solution_Count + 1;
            Put_Line (Output_File, X_Coordinates'Last * '-');
         end loop; -- S in Iterate (Solution_Set)
      end Put;

      procedure Verify_Solution (Grid : in Grids;
                                 Word_List_In : Word_Lists;
                                 Fill_List : In Fill_Lists.Vector) is

         -- Provides an independent check that the solution is correct.
         -- Raises Veification_Error if the solution is invalid.

         Word_List : Word_Lists := Word_List_In;
         Grid_Read_Back : Unbounded_String;
         X : X_Coordinates;
         Y : Y_Coordinates;
         All_Placed : Boolean := True;

      begin -- Verify_Solution
         Put_Line (Debug_File, "Verification of solution");
         for I in Word_Indices loop
            Word_List (I).Used := False;
         end loop; -- I in Word_Indices
         for F in Iterate (Fill_List) loop
            if Word_List (Fill_List (F).Word_index).Used then
               raise Verification_Error with """" &
                 To_String (Word_List (Fill_List (F).Word_index).Word) &
                 """ placed more than once";
            end if; -- Word_List (Fill_List (I).Word_index).Used
            Word_List (Fill_List (F).Word_index).Used := True;
            Grid_Read_Back := Null_Unbounded_String;
            X := Fill_List (F).X;
            Y := Fill_List (F).Y;
            for P in Letter_Positions range
              1 .. Fill_List (F).Word_Length loop
               if Fill_List (F).Direction = Across then
                  X := Fill_List (F).X + P - 1;
               else
                  Y := Fill_List (F).Y + P - 1;
               end if; -- Fill_List (F).Direction = Across
               Grid_Read_Back := Grid_Read_Back & Grid (X, Y).Ch;
            end loop; -- P in Letter_Positions range ...
            if Word_List (Fill_List (F).Word_index).Word /=
              Grid_Read_Back then
               raise Verification_Error with "Word """ &
                 To_String (Word_List (Fill_List (F).Word_index).Word) &
                 """ does not match (" &
                 X_Coordinates'Image ( Fill_List (F).X) & ',' &
                 Y_Coordinates'Image ( Fill_List (F).Y) & "): """ &
                 To_String (Grid_Read_Back) & '"';
            end if; -- Word_List (Fill_List (F).Word_index).Word /= ...
         end loop; -- F in Iterate (Fill_List) loop
         Verify_Fill_List (Word_List, Fill_List);
         for I in Word_Indices loop
            All_Placed := All_Placed and Word_List (I).Used;
         end loop; -- I in Word_Indices
         If not All_Placed then
            raise Verification_Error with "Some words not placed";
         end if; -- not All_Placed
         Put_Line (Debug_File, "Verification of solution complete");
      end Verify_Solution;

      procedure Search (Grid_In : in Grids;
                        Word_List_In : in Word_Lists;
                        Length_List_In : in Length_Lists;
                        Fill_List_In : in Fill_Lists.Vector;
                        Word_Map : in Word_Maps.Map;
                        Solution_Set : in out Solution_Sets.Set;
                        Depth : in Natural) is

         -- Recursive search for a solution. Solve puzzles which do not have
         -- unique solutions and will find all solutions. Can potentialy solve
         -- puzzles without starting words and puzzles where all the rules fail
         -- to make progress, that is where it is necessary to try a word
         -- placement and test if that leads to a solution.

         Grid : Grids := Grid_In;
         Word_list : Word_Lists := Word_List_In;
         Fill_List : Fill_Lists.Vector := Fill_List_In;
         Length_List : Length_Lists := Length_List_In;
         F : Fill_List_Indices := Fill_List_Indices'First;
         Placement_Set : Word_Sets.Set := Word_Sets.Empty_Set;
         X : X_Coordinates;
         Y : Y_Coordinates;
         All_Placed : Boolean := True;
         Unused_Found : Boolean := False;

      begin -- Search
         Put_Line (Debug_File, "Search Depth:" & Natural'Image (Depth));
         Put ( Debug_File, Grid);
         loop -- iterate over Fill_List
            if not Fill_List (F).Used then
               Unused_Found := True;
               All_Placed := False;
               Fill_List (F).Used := True;
               -- Search is not complete
               Check_One_Direction (Grid, Word_List, Length_List, Fill_List,
                                    Word_Map, F, Placement_Set);
               if not Is_Empty (Placement_Set) then
                  -- Even a unique choice at this point could be erroneous
                  -- because of a previous choice which does not lead to a
                  -- solution.
                  Negation_Check (Grid, Word_List, Length_List, Fill_List,
                                  Word_Map, F, Placement_Set);
               end if; -- not Is_Empty (Placement_Set)
               X := Fill_List (F).X;
               Y := Fill_List (F).Y;
               -- The Placement_List may be empty which indicates that there is
               -- no solution to be found by a deeper decent.
               for I in Iterate (Placement_Set) loop
                  Fill_List (F).Word_Index := Element (I);
                  Word_List (Element (I)).Used := True;
                  Exclude (Length_List (Fill_List (F).Word_Length),
                           Element (I));
                  for P in Letter_Positions range
                    1 .. Fill_List (F).Word_Length loop
                     if Fill_List (F).Direction = Across then
                        X := Fill_List (F).X + P - 1;
                     else
                        Y := Fill_List (F).Y + P - 1;
                     end if; -- Fill_List (F).Direction = Across
                     if Grid (X, Y).Ch /= Open_Ch and then Grid (X, Y).Ch /=
                       Element (Word_List (Element (I)). Word, P) then
                        raise Search_Error with "Fill_List (" &
                          Fill_List_Indices'Image (F) & "): overwriting '" &
                          Grid (X, Y).Ch & "' with '" &
                          Element (Word_List (Element (I)). Word, P) &
                          "' when placing " &
                          To_String (Word_List (Element (I)). Word);
                     end if; -- Grid (X, Y).Ch /= Open_Ch and then ...
                     Grid (X, Y).Ch :=
                       Element (Word_List (Element (I)). Word, P);
                  end loop; -- P in Letter_Positions range ...
                  Put_Line (Debug_File, "F:" & Fill_List_Indices'Image (F) &
                            ' ' & Word_List (Element (I)).Word);
                  -- Continue search with updated data structures
                  Search (Grid, Word_List, Length_List, Fill_List, Word_Map,
                          Solution_Set, Depth + 1);
                  -- Restore previous state of data structures for next
                  -- iteration
                  Word_List (Element (I)).Used := False;
                  Include (Length_List (Fill_List (F).Word_Length),
                           Element (I));
                  Grid := Grid_In;
               end loop; -- I in Iterate (Placement_Set)
            end if; -- not Fill_List (F).Used
            -- Exits loop after first Fill_List element that can be filled
            -- is processed, avoids the same solution being found multiple
            -- times.
            F := F + 1;
            exit when Unused_Found or F > Last_Index (Fill_List);
         end loop; -- F in Iterate (Fill_List)
         if All_Placed then
            -- Solved
            Put_Line (Debug_File, "Solution Found");
            Save_Solution (Grid, Solution_Set);
            Verify_Solution (Grid, Word_list, Fill_List);
         end if; -- All_Placed
         Put_Line (Debug_File, "Unwind depth:" & Natural'Image (Depth));
      exception
         when Search_Error | Verification_Error =>
            Put_Line (Debug_File, "**** Start of Search Level" &
                        Natural'Image (Depth) & " Exception Dump ****");
            Put ( Debug_File, Grid);
            Debug_Data_Structures (Grid, Word_List, Length_List, Fill_List,
                                   Word_Map);
            raise;
      end Search;

      procedure Update (Grid : in out Grids;
                        Word_List : in out Word_Lists;
                        Length_List : in out Length_Lists;
                        Fill_List : in out Fill_Lists.Vector;
                        F : in Fill_List_Indices;
                        Word_Index : in Word_Indices;
                        Update_Type : in Update_Types) is

         -- Updates the GRID, Fill_List and Word_List when a word is placed.
         -- It also updates the sceeen.

         X : X_Coordinates := Fill_List (F).X;
         Y : Y_Coordinates := Fill_List (F).Y;

      begin -- Update
         Fill_List (F).Used := True;
         Fill_List (F).Word_Index := Word_Index;
         Word_List (Word_Index).Used := True;
         Exclude (Length_List (Fill_List (F).Word_Length), Word_Index);
         FL_IO.Put (Debug_File, F, 3);
         Put_Line (Debug_File, ' ' & Update_Type & ' ' &
                     Word_List (Word_Index).Word);
         for P in Letter_Positions range 1 .. Fill_List (F).Word_Length loop
            if Fill_List (F).Direction = Across then
               X := Fill_List (F).X + P - 1;
            else
               Y := Fill_List (F).Y + P - 1;
            end if; -- Fill_List (F).Direction = Across
            if Grid (X, Y).Ch = Open_Ch or else
              Grid (X, Y).Ch = Element (Word_List (Word_Index).Word, P) then
               Grid (X, Y).Ch := Element (Word_List (Word_Index).Word, P);
               Put (Grid, X, Y);
            else
               raise Update_Error with "Fill_List (" &
                 Fill_List_Indices'Image (F) & ") overwriting '" &
                 Grid (X, Y).Ch & "' with '" &
                 Element (Word_List (Word_Index).Word, P) &
                 "' when placing """ &
                 To_String (Word_List (Word_Index).Word) &
                 """ Update_Type: " & Update_Type;
            end if; --  Grid (X, Y).Ch = Open_Ch or else ...
         end loop; -- P in Letter_Positions range 1 ...
         Goto_XY (X_Pos'Last, Y_Pos'Last);
         delay Word_Time;
      end Update;

      function Words_Not_Placed (Word_List : in Word_Lists) return Natural is

         Result : Natural := 0;

      begin -- Words_Not_Placed
         for W in Word_Indices loop
            if not Word_List (W).Used then
               Result := Result + 1;
            end if; -- not Word_List (W).Used
         end loop; -- W in Word_Indices
         return Result;
      end Words_Not_Placed;

      procedure Queued_Place_Words (Grid : in out Grids;
                                    Word_List : in out Word_Lists;
                                    Length_List : in out Length_Lists;
                                    Fill_List : in out Fill_Lists.Vector;
                                    Word_Map : in Word_Maps.Map) is

         -- This procedure uses change of state processing based on a queue.
         -- When a word is placed all the Fill_List entries which contain
         -- Grid cells in that word are queued. The process continues until
         -- the queue is empty or there has been no progress in one full pass
         -- through the queue. The no progress condition is determined from the
         -- number of queue elements processed and a Progress_Limit which was
         -- calculated when the most recently placed word was placed. The
         -- Progress_Limit is recalculated when a word is placed or if the queue
         -- is refreshed. On the first occasion no progress is detected the
         -- queue is refreshed with all the unused Fill_List entries. This will
         -- allow a complete solution to be found if there is only a single word
         -- of N letters and a single empty Fill_List element of lenght N. If
         -- the no progress condition occurs a second time, Search is invoked to
         -- complete the solution. An empty queue is of indicative of the puzzle
         -- being solved.

         package Fill_Queue_Interfaces is new
           Ada.Containers.Synchronized_Queue_Interfaces (Fill_List_Indices);

         package Fill_Queues is new
           Ada.Containers.Unbounded_Synchronized_Queues (Fill_Queue_Interfaces);

         Fill_Queue : Fill_Queues.Queue;

         procedure Update_Queue (Grid : in Grids;
                                 Fill_List : in Fill_Lists.Vector;
                                 Fq : in Fill_List_Indices) is

            X : X_Coordinates := Fill_List (Fq).X;
            Y : Y_Coordinates := Fill_List (Fq).Y;
            D : Directions;

         begin -- Update_Queue
            if Fill_List (Fq).Direction = Across then
               D := Down;
            else
               D := Across;
            end if; -- Fill_List (F).Direction = Across
            for P in Letter_Positions range 1 .. Fill_List (Fq).Word_Length loop
               if Fill_List (Fq).Direction = Across then
                  X := Fill_List (Fq).X + P - 1;
               else
                  Y := Fill_List (Fq).Y + P - 1;
               end if; -- Fill_List (Fq).Direction = Across
               if Grid (X, Y).Cross_Array (D) /= No_Index  and then
                 not Fill_List (Grid (X, Y).Cross_Array (D)).Used then
                  Fill_Queue.Enqueue (Grid (X, Y).Cross_Array (D));
               end if; --  Grid (X, Y).Cross_Array (D) /= No_Index  and then
            end loop; -- P in Letter_Positions range 1 ...
         end Update_Queue;

         Fq : Fill_List_Indices;
         Element_Count : Count_Type := 0;
         Placement_Set : Word_Sets.Set := Word_Sets.Empty_Set;
         Solution_Set : Solution_Sets.Set := Solution_Sets.Empty_Set;
         Update_Type : Update_Types;
         Refresh : Boolean := True;
         Progress_Limit, Previous_Use : Count_Type;

      begin -- Queued_Place_Words
         Put_Line (Debug_File, "Queued solution strategy");
         Put ( Debug_File, Grid);
         -- initialise Fill_Queue based on intial word
         for F in Fill_List_Indices range 1 .. Last_Index (Fill_List) loop
            if Fill_List (F).Used then
               Update_Queue (Grid, Fill_List, F);
            end if; -- Fill_List (F).Used
         end loop; -- F in Fill_List_Indices range 1 .. Last_Index (Fill_List)
         -- Progress_Limit := Positive (2 * Fill_Queue.Current_Use);
         Progress_Limit := Fill_Queue.Current_Use + 1;
         Put_Line (Debug_File, "Queue initialised with" &
                     Count_Type'Image (Fill_Queue.Current_Use) &
                     " elements");
         while Fill_Queue.Current_Use > 0 and
           Element_Count <= Progress_Limit loop
            Fill_Queue.Dequeue (Fq);
            Element_Count := Element_Count + 1;
            if not Fill_List (Fq).Used then
               -- Check in one direction, looking for a unique word to
               -- satisfy letters within the current Fill_List element
               Check_One_Direction (Grid, Word_list, Length_List, Fill_List,
                                    Word_Map, Fq, Placement_Set);
               Update_Type := 'o';
               if Length (Placement_Set) > 1 then
                  -- Attempt to remove any words for which no cross word
                  -- can be found that has a common letter at the
                  -- intersection
                  Negation_Check (Grid, Word_List, Length_List, Fill_List,
                                  Word_Map, Fq, Placement_Set);
                  Update_Type := 'n';
               end if; -- Length (Placement_Set) > 1
               if Length (Placement_Set) = 1 then
                  -- unique word, place word
                  Update (Grid, Word_List, Length_List, Fill_List, Fq,
                          First_Element (Placement_Set), Update_Type);
                  Update_Queue (Grid, Fill_List, Fq);
                  Progress_Limit := Element_Count + Fill_Queue.Current_Use + 1;
               else
                  -- Requwuw the Fill_List element
                  Fill_Queue.Enqueue (Fq);
               end if; -- Length (Placement_Set) = 1
            end if; -- not Fill_List (Fq).Used
            if Element_Count = Progress_Limit then
               if Refresh then
                  Refresh := False;
                  -- Only one refresh allowed, now revert to something similar
                  -- to iterative placement
                  Previous_Use := Fill_Queue.Current_Use;
                  for F in Fill_List_Indices range 1 ..
                    Last_Index (Fill_List) loop
                     if not Fill_List (F).Used then
                        Fill_Queue.Enqueue (F);
                     end if; -- not Fill_List (F).Used
                  end loop; -- F in Fill_List_Indices range 1 ...
                  Put_Line (Debug_File, "Queue refreshed with" &
                              Count_Type'Image (Fill_Queue.Current_Use -
                                Previous_Use) & " elements");
                  Progress_Limit := Element_Count + Fill_Queue.Current_Use + 1;
               end if; -- Refresh
            end if; -- Element_Count = Progress_Limit
         end loop; -- Fill_Queue.Current_Use > 0 and ...
         Goto_XY (X_Pos'First, Y_Coordinates'Last);
         Put ("Elements Processed:" & Count_Type'Image (Element_Count));
         Put (Debug_File,
              "Elements Processed:" & Count_Type'Image (Element_Count));
         if Words_Not_Placed (Word_List) = 0 then
            Put_Line (Debug_File, " All words placed");
            Verify_Solution (Grid, Word_List, Fill_List);
            -- If verification fails the screen display will not show all words
            -- placed.
            Put (" All words placed");
            Put_Line (Solution_File, "Solution");
            Put (Solution_File, Grid);
         else
            Put_Line (Debug_File, "Solution");
            Put (Debug_File, Grid);
            Put (" Queue length:" &
                   Count_Type'Image (Fill_Queue.Current_Use));
            Put (Debug_File, " Queue length:" &
                   Count_Type'Image (Fill_Queue.Current_Use));
            Put (" Words not placed:" &
                   Natural'Image (Words_Not_Placed (Word_List)));
            Put_Line (Debug_File, " Words not placed:" &
                        Natural'Image (Words_Not_Placed (Word_List)));
            Debug_Data_Structures (Grid, Word_List, Length_List, Fill_List,
                                   Word_Map);
            delay Solution_Time;
            Put_Line (Debug_File, "Starting Search");
            Search (Grid, Word_List, Length_List, Fill_List, Word_Map,
                    Solution_Set, 0);
            Display (Solution_Set);
            Put (Solution_File, Solution_Set);
         end if; -- Words_Not_Placed (Word_List) = 0
      end Queued_Place_Words;

      procedure Iterative_Place_Words (Grid : in out Grids;
                                       Word_List : in out Word_Lists;
                                       Length_List : in out Length_Lists;
                                       Fill_List : in out Fill_Lists.Vector;
                                       Word_Map : in Word_Maps.Map) is

         -- This procedure repetedly iterates over the Fill_List applying
         -- Check_One_Direction and Check_Negation in an attempt to solve the
         -- puzzle by finding a unique word for each Fill_List element. The
         -- termination criteria are either there is no progress, that is, no
         -- word is placed in one complete pass through the Fill_List. When
         -- there is no progress and words still to placed, Search is invoked to
         -- find a complete the solution. Alternatively if all words have been
         -- placed the solution is completed.

         Progress, All_Placed : Boolean;
         Placement_Set : Word_Sets.Set := Word_Sets.Empty_Set;
         Pass_Count : Positive := 1;
         Solution_Set : Solution_Sets.Set := Solution_Sets.Empty_Set;
         Update_Type : Update_Types;

      begin -- Iterative_Place_Words
         Put_Line (Debug_File, "Iterative solution strategy");
         loop -- repeat attempting to place words
            Put ( Debug_File, Grid);
            Put_Line (Debug_File, "Start Pass:" & Positive'Image (Pass_Count));
            Progress := False;
            All_Placed := True;
            for F in Fill_List_Indices range 1 .. Last_Index (Fill_List) loop
               if not Fill_List (F).Used then
                  -- Check in one direction, looking for a unique word to
                  -- satisfy letters within the current Fill_List element
                  Check_One_Direction (Grid, Word_list, Length_List, Fill_List,
                                       Word_Map, F, Placement_Set);
                  Update_Type := 'o';
                  if Length (Placement_Set) > 1 then
                     -- Attempt to remove any words for which no cross word
                     -- can be found that has a common letter at the
                     -- intersection
                     Negation_Check (Grid, Word_List, Length_List, Fill_List,
                                     Word_Map, F, Placement_Set);
                     Update_Type := 'n';
                  end if; -- Length (Placement_Set) > 1
                  if Length (Placement_Set) = 1 then
                     -- unique word, place word
                     Progress := True;
                     Update (Grid, Word_List, Length_List, Fill_List, F,
                             First_Element (Placement_Set), Update_Type);
                  else
                     -- zero or greater than one word
                     All_Placed := False;
                  end if; -- Length (Placement_Set) = 1
               end if; -- not Fill_List (F).Used
            end loop; -- Fill_List_Indices range 1 .. Last_Index (Fill_List)
            Put_Line (Debug_File, "Progress: " & Boolean'Image (Progress) &
                        " All_Placed: " & Boolean'Image (All_Placed));
            exit when not Progress or All_Placed;
            Goto_XY (X_Pos'First, Y_Coordinates'Last);
            Put ("Pass:" & Positive'Image (Pass_Count));
            Pass_Count := Pass_Count + 1;
            delay Pass_Time;
         end loop; -- repeat attempting to place words
         Goto_XY (X_Pos'First, Y_Coordinates'Last);
         Put ("Passes:" & Positive'Image (Pass_Count));
         Put (Debug_File, Grid);
         Put (Debug_File, "Passes:" & Positive'Image (Pass_Count));
         if All_Placed then
            Put_Line (Solution_File, "Solution");
            Put (Solution_File, Grid);
            Put_Line (Debug_File, " All words placed");
            Verify_Solution (Grid, Word_List, Fill_List);
            -- If verification fails the screen display will not show all words
            -- placed.
            Put (" All words placed");
         else
            Put (" Words not placed:" &
                   Natural'Image (Words_Not_Placed (Word_List)));
            Put_Line (Debug_File, " Words not placed:" &
                        Natural'Image (Words_Not_Placed (Word_List)));
            Debug_Data_Structures (Grid, Word_List, Length_List, Fill_List,
                                   Word_Map);
            delay Solution_Time;
            Put_Line (Debug_File, "Starting Search");
            Search (Grid, Word_List, Length_List, Fill_List, Word_Map,
                    Solution_Set, 0);
            Display (Solution_Set);
            Put (Solution_File, Solution_Set);
         end if; -- All_Placed
      end Iterative_Place_Words;

      Grid : Grids;
      Word_List : Word_Lists;
      Length_List : Length_Lists;
      Fill_List : Fill_Lists.Vector;
      Word_Map : Word_Maps.Map;
      Solution_Set : Solution_Sets.Set;

   begin -- Solve;
      Create (Debug_File, Out_File, Argument (1) & "_Debug.txt");
      Put_Line (Debug_File, "Command: " & Command_Name & ' ' & Argument (1));
      Read_Word_List (List_File, Word_List);
      Build_Length_List (Word_List, Length_List);
      Build_Word_Map (Word_List, Word_Map);
      if Solution_Mode /= Map then
         Create (Solution_File, Out_File, Argument (1) &
                   "_Solution.txt");
         Clear_Screen;
         Read_Grid (Grid_File, Grid);
         Build_Fill_List (Grid, Word_List, Fill_List);
         Build_Cross_Reference (Grid, Fill_List);
         Debug_Data_Structures (Grid, Word_List, Length_List, Fill_List,
                                Word_Map);
      end if; -- Solution_Mode /= Map
      case Solution_Mode is
         when Iterative =>
            Iterative_Place_Words (Grid, Word_List, Length_List, Fill_List,
                                   Word_Map);
         when Queued =>
            Queued_Place_Words (Grid, Word_List, Length_List, Fill_List,
                                Word_Map);
         when Recursive =>
            Put_Line (Debug_File, "Starting Search");
            Search (Grid, Word_List, Length_List, Fill_List, Word_Map,
                    Solution_Set, 0);
            Display (Solution_Set);
            Put (Solution_File, Solution_Set);
         when Map =>
            Put_Line ("Writing Word Map to " & Name (Debug_File));
            Put_Line (Debug_File, "Computer aided solution");
            Put_Map (Word_List, Word_Map);
      end case; -- Solution_Made
      if Solution_Mode /= Map then
         Close (Solution_File);
      end if; -- Solution_Mode /= Map
      Close (Debug_File);
   exception
      when  Fill_List_Error | Update_Error | Unique_Error |
           Verification_Error =>
         Put_Line (Debug_File, "**** Start of Exception Dump ****");
         Debug_Data_Structures (Grid, Word_List, Length_list, Fill_list,
                                Word_Map);
         Put (Debug_File, Grid);
         Goto_XY (X_Pos'First, Y_Coordinates'Last);
         raise;
   end Solve;

   Default_Width : constant Positive := 80; -- characters
   Default_Height : constant Positive := 25; -- lines

   X_Limit, Y_Limit : Natural;
   X_Screen : Positive := Default_Width;
   Y_Screen : Positive := Default_Height;
   Grid_File : File_Type;
   List_File : File_Type;
   Word_Count : Positive;
   Shortest_Word, Longest_Word : Positive;
   Solution_Mode : Solution_Modes := Queued;
   -- Default to queued mode.
   Enable_Fill_List_Exceptions : Boolean := True;

begin -- WordFit
   if Argument_Count > 0 then
      for I in Natural range 2 .. Argument_Count loop
         case Argument (I) (1) is
            when 'c' | 'C' =>
               Enable_Fill_List_Exceptions := False;
            when 'i' | 'I' =>
               Solution_Mode := Iterative;
            when 'm' | 'M' =>
               Solution_Mode := Map;
            when 'q' | 'Q' =>
               Solution_Mode := Queued;
            when 'r' | 'R' =>
               Solution_Mode := Recursive;
            when others =>
               null;
         end case; -- Argument (I) (1)
      end loop; -- I in Natural range 2 .. Argument_Count
      Open (List_File, In_File, Argument (1) & List_Name);
      Find_Limits (List_File, Word_Count, Shortest_Word, Longest_Word);
      if Solution_Mode = Map then
         X_Limit := 1;
         Y_Limit := 1;
         -- required to pass in-range values to Solve, not used in Map mode
         Solve (Grid_File, List_File, X_Limit, Y_Limit, X_Screen, Y_Screen,
                Word_Count, Shortest_Word, Longest_Word, Solution_Mode,
                Enable_Fill_List_Exceptions);
      else
         Open (Grid_File, In_File, Argument (1) & Grid_Name);
         Find_Limits (Grid_File, X_Limit, Y_Limit);
         if X_limit - 1 > Default_Width then
            X_Screen := X_Limit - 1;
         end if; -- X_limit > Default_Width
         if Y_limit > Default_Height then
            Y_Screen := Y_Limit;
         end if; -- Y_limit > Default_Height
         Solve (Grid_File, List_File, X_Limit, Y_Limit, X_Screen, Y_Screen,
                Word_Count, Shortest_Word, Longest_Word, Solution_Mode,
                Enable_Fill_List_Exceptions);
         Close (Grid_File);
      end if; -- Solution_Mode /= Map
      Close (List_File);
   else
      Put_Line ("Usage: WordFit Base_File_Name {i | m | q | r } {c}");
      New_Line;
      Put_Line ("The optional switches are as follows:");
      Put_Line ("'i' : Solve by Iteration");
      Put_Line ("'m' : Computer assistance only, word Map produced");
      Put_Line ("'q' : Solve on change of state basis using a Queue");
      Put_Line ("'r' : Solve by Recursion");
      Put_Line ("'c' : Continue to solve if there is a mismatch between the");
      Put_Line ("      mumber of words to place and the spaces available");
      New_Line;
      Put_Line ("Notes:");
      Put_Line ("(1) When 'm' switch is used, only a *" & List_Name &
                  " file is required.");
      Put_Line ("(2) When the 'i' or 'q' switches are used to solve a puzzle");
      Put_Line ("    which has multiple solutions, Recursion is used to");
      Put_Line ("    obtain complete solutions.");
      Put_Line ("(3) Recursion displays every distinct solution. If the same");
      Put_Line ("    solution is found via different search paths, it is only");
      Put_Line ("    counted and displayed once.");
      Put_Line ("(4) If the input files do not have a solution, either an");
      Put_Line ("    error message will be given or the solver will");
      Put_Line ("    terminate with an incomplete solution.");
   end if; -- Argument_Count > 0
end WordFit;
