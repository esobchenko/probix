-record(error, {code, message}).
-record(series, {id, time_created, label}).
%% tick id is a composite key which includes series id and tick timestamp: {Id, Timestamp}
-record(tick, {id, value}).
