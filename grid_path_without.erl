-module(grid_path_without).
-export([grid_path/4]).

% Functin which will be called from shell to find the path
grid_path(NoRows, NoCols, {SrcRow, SrcCol}, {DestRow, DestCol}) ->
    grid_path(NoRows, NoCols, {SrcRow, SrcCol}, {DestRow, DestCol}, []).

grid_path(_, _, {DestRow, DestCol}, {DestRow, DestCol}, Acc) ->
    % Destination reached
    lists:reverse([{DestRow, DestCol} | Acc]);

grid_path(NoRows, NoCols, {SrcRow, SrcCol}, {DestRow, DestCol}, Acc) ->
    % Move vertically or horizontally
    {NewSrcRow, NewSrcCol} = if SrcRow /= DestRow ->
                               {SrcRow + sign(DestRow - SrcRow), SrcCol};
                             true ->
                               {SrcRow, SrcCol + sign(DestCol - SrcCol)}
                           end,
    grid_path(NoRows, NoCols, {NewSrcRow, NewSrcCol}, {DestRow, DestCol}, [{SrcRow, SrcCol} | Acc]).

% Helper function to determine the sign of a number
sign(X) when X > 0 -> 1;
sign(X) when X < 0 -> -1;
sign(_) -> 0.
