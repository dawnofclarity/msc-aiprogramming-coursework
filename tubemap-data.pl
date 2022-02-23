%
% London Underground Route Planner Database
%
% Dawn Raison (6609229)
%

% lines
% line(++atom, ++name).
line(lineJubilee, 'Jubilee Line').
line(lineVictoria, 'Victoria Line').
line(lineNorthern, 'Northern Line').
line(lineBakerloo, 'Bakerloo Line').
line(lineDistrict, 'District Line').
line(lineCentral, 'Central Line').
line(linePiccadilly, 'Piccadilly Line').

% Stations
% station(++atom, ++name, ++x, ++y).
% x and y are notional Cartesian co-ords of each station, used to determine
% approximate the "as the crow flies" distance between stations.
station(stnBakerStreet, 'Baker Street', 3, 3).
station(stnBondStreet, 'Bond Street', 3, 8).
station(stnGreenPark, 'Green Park', 4, 13).
station(stnWestminster, 'Westminster', 9, 18).
station(stnWaterloo, 'Waterloo', 12, 21).
station(stnMarbleArch, 'Marble Arch', 1, 10).
station(stnHydeParkCorner, 'Hyde Park Corner', 2, 14).
station(stnVictoria, 'Victoria', 4, 18).
station(stnEmbankment, 'Embankment', 12, 18).
station(stnCharingCross, 'Charing Cross', 12, 16).
station(stnPiccadillyCircus, 'Piccadilly Circus', 9, 13).
station(stnOxfordCircus, 'Oxford Circus', 7, 8).
station(stnLeicesterSquare, 'Leicester Square', 12, 11).
station(stnTottenhamCourtRoad, 'Tottenham Court Road', 12, 8).
station(stnWarrenStreet, 'Warren Street', 12, 4).
station(stnHolborn, 'Holborn', 15, 8).
station(stnGoodgeStreet, 'Goodge Street', 12, 6).
station(stnCoventGarden, 'Covent Garden', 13, 10).
station(stnEuston, 'Euston', 13, 2).
station(stnRegentsPark, 'Regent\'s Park', 4, 4).
station(stnStJamesPark, 'St James\'s Park', 7, 18).

% Links between stations
% link(++start, ++end, ++line, ++duration).
link(stnBakerStreet, stnBondStreet, lineJubilee, 2).
link(stnBondStreet, stnGreenPark, lineJubilee, 2).
link(stnGreenPark, stnWestminster, lineJubilee, 2).
link(stnWestminster, stnWaterloo, lineJubilee, 2).

link(stnWaterloo, stnEmbankment, lineNorthern, 1).
link(stnEmbankment, stnCharingCross, lineNorthern, 1).
link(stnCharingCross, stnLeicesterSquare, lineNorthern, 2).
link(stnLeicesterSquare, stnTottenhamCourtRoad, lineNorthern, 1).
link(stnTottenhamCourtRoad, stnGoodgeStreet, lineNorthern, 2).
link(stnGoodgeStreet, stnWarrenStreet, lineNorthern, 1).
link(stnWarrenStreet, stnEuston, lineNorthern, 1).

link(stnEuston, stnWarrenStreet, lineVictoria, 1).
link(stnWarrenStreet, stnOxfordCircus, lineVictoria, 2).
link(stnOxfordCircus, stnGreenPark, lineVictoria, 2).
link(stnGreenPark, stnVictoria, lineVictoria, 2).

link(stnHydeParkCorner, stnGreenPark, linePiccadilly, 2).
link(stnGreenPark, stnPiccadillyCircus, linePiccadilly, 2).
link(stnPiccadillyCircus, stnLeicesterSquare, linePiccadilly, 1).
link(stnLeicesterSquare, stnCoventGarden, linePiccadilly, 1).
link(stnCoventGarden, stnHolborn, linePiccadilly, 2).

link(stnMarbleArch, stnBondStreet, lineCentral, 2).
link(stnBondStreet, stnOxfordCircus, lineCentral, 1).
link(stnOxfordCircus, stnTottenhamCourtRoad, lineCentral, 2).
link(stnTottenhamCourtRoad, stnHolborn, lineCentral, 1).

link(stnWaterloo, stnEmbankment, lineBakerloo, 2).
link(stnEmbankment, stnCharingCross, lineBakerloo, 1).
link(stnCharingCross, stnPiccadillyCircus, lineBakerloo, 1).
link(stnPiccadillyCircus, stnOxfordCircus, lineBakerloo, 2).
link(stnOxfordCircus, stnRegentsPark, lineBakerloo, 3).
link(stnRegentsPark, stnBakerStreet, lineBakerloo, 1).

link(stnEmbankment, stnWestminster, lineDistrict, 1).
link(stnWestminster, stnStJamesPark, lineDistrict, 2).
link(stnStJamesPark, stnVictoria, lineDistrict, 2).

% Make bidirectional data facade to save writing all data lines twice!
linked(S, E, L, D) :- link(S, E, L, D).
linked(S, E, L, D) :- link(E, S, L, D).

% Configuration items
% config(++Key, ++Value).
config('changeLine', 5).
config('avSpeed', 2).

