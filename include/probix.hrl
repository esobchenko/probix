-record(error, {code, message}).
-record(series, {id, time_created}).
%% pobe id is a composite key which includes object id and probe timestamp: {Id, Timestamp}
-record(probe, {id, value}).
