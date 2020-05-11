WordFit and Blank_Files

The WordFit solves puzzles similar to crosswords where there is a grid with some
cells blocked out and others occupied by one or more initial words. The grid is
to be populated by words in a list. Word fit takes takes two files as input
Base_Name_Grid.txt and Base_Name_List.txt, the base name is provided as a
command line parameter e.g. wordfit CM_20200504. The solution is displayed on
the screen and and written to Base_Name_Debug.txt along with debugging
information which may help in determining why no solution was found.

Optional command line switches i, m, q or r can be used to select the process
used to solve the puzzle. With the i switch iteration is used. with the m
switch, a word map only is produced in the debug file (Manual computer aided
solution). The q switch process on change of state basis queueing a location to
be filled when each new letter is placed. The r switch uses recursion to search
for one or more solutions. If the i or q switches fail to solve the puzzle
recursion is invoked. This will occur if the puzzle has multiple or no solution.
If no switch is supplies the queue solution is invoked. For example the
following would use iteration: wordfit CM_20200504 i

Blank_Files creates a skeletal grid file and a word list file which can then be
edited with your favorite text editor. It takes three parameters, Base_Name X Y
which creates a grid file Base_Name_Grid.txt which is X characters wide by Y
characters high and a list file Base_Name_List.txt. To facilitate the use of OCR
Blank_Files can read files of the form Base_Name_List_m.txt (where m is 0,
1, ...). If files of the form Base_Name_List_m.txt exist the list file is
populated from the their content. Content of the form "nn LETTERS" (where nn is
a positive number) will be not transferred to the list file. If no files are
read an empty list file will be created.

For example Blank_Files CM_20200505 17 17 would produce a grid file
CM20200505_Grid.txt which is 17 characters square and a list file
CM20200505_List.txt.

The skeletal grid file contains digits in the header, sides and footer to act as
a ruler to aid in editing (overwriting spaces etc.). The boarder characters are
used by Word_Fit to check the integrity of the grid file.

Example input files from the Courier Mail (Brisbane daily newspaper) are
included. Note CM_20200422 has four solutions.

WordFit is dependent on NT_Console, which is in a separate repository. This
supplies basic screen manipulation similar to the old DOS ANSI.sys driver. The
version here is a generic version of a similarly named package written by Jerry
van Dijk whose original copyright notice appears in the files.
