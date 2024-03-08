/* Order / flow for parsing to/from documents:
1) The problem starts as a file with the following defined (either absolute or relative):
  > All relevant points (either absolute or relative)
  > All applied loads w/ known pos & magnitude
  > All supports (labled as either internal or external)
  > One point specified to be the base point
2) The problem is parsed into a list of points w/ known positions & unique IDs / names
  > Each relevant point in the problem (any points + any point where a force, moment, or load acts) has
    a known absolute position (in the coordinate system) and a unique ID
  > There are no duplicate points (e.g. if two things are at the same point, they have the same point ID)
3) A list of forces, loads, moments, and supports is parsed from the list of points
  > Each one knowns where it acts on
4) The list of loads (etc.) is converted into lists of each force and moment
  > The force / moment vectors all have a known origin point
  > Each vector is represented component wise and each component is either known or unknown
  > There is no difference between the reactions and applied loads (although is is possible
    to see where each vector came from)
5) The lists are split into seperate pieces where the equations of equilibrium can be applied
  > Forces are split up non-uniquely (e.g. the same force can be in more than one piece)
  > Each piece represents one body
6) Each body is solved via conversion to a matrix equation
7) The results are pulled from the individual rigid bodies and compaired / checked
8) The solved values are put back into the lists & an output file is generated
  > The generated output file has all the resolved information (e.g. point positions & vector components)
  > The names / order is the same as the input file
*/
