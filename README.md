# msc-aiprogramming-coursework
Prolog routmapping - London Underground

This demonstrates the use of Prolog to implement a Route planner for navigating between stations on the London Underground using A* search algorithm.
A filter is sincluded that converts the raw route output from the search into a more useful set of English instructions.

## Algorithm
The route search is implemented around an A* search to derive the fastest route between any two stations, taking into account a configurable time penalty should the journey require line changes.

Two main entry points are defined:
* Search/3 Conducts the route search and returns the result in a variable.
* Search/2 Conducts the route search, then pretty prints it in a human friendly format to the output stream. Internally this invokes search/3 and passes the result to pretty_print_route/2.

The implementation as supplied only returns the best route; however by removing the cut in process_node, backtracking can be used to produce multiple hypotheses in decreasing optimality

## Database
A sample database, developed using a small section of the full underground map, is supplied for evaluation.

The map is held internally as a graph of nodes, where each node represents a station, and the links between the nodes represent the underground lines.

Each underground line is represented by a fact that links an atom (prefixed line) to a human readable text.

Each station likewise is represented by a fact linking an atom (prefixed stn), to a textual name, and its relative (x, y) co-ordinates.

A set of link facts denote the links between stations, which line makes the link, and the duration of transit taken to pass between the stations.

A pair of linked clauses creates a façade used to represent the links in both directions, to avoid having to specify all the links explicitly.

This optimisation makes the assumption that travel times are identical in both directions, which is acceptable in this case, but might require further consideration for a real application.

Two configuration items also form part of the database.
* The first configurable fact config('changeLine', 5) is used to define a time penalty applied each time the journey requires the user to change underground line. In this instance it is assumed a constant regardless of the lines involved, but this could be expanded based on real world data.
* The second configurable fact config('avSpeed', 2) is used to deduce the time remaining to reach the target station.
