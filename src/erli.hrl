-define(FLAG_LIMIT, 5). % number of reports before Target URLs are banned

-define(STAT_COLLECT_INTERVAL, 6000). % this breaks statistics... hardcore
-define(MAX_CONFLICTS, 5).
%-define(STAT_COLLECT_INTERVAL, 3600000). % time in ms between calls to the parser script
%-define(MAX_CONFLICTS, 100). % the maximal number of attempts to generate a new URL path

-define(SCRIPT_NAME, "parse_eval.py").

-record(target, {target, paths=[], reported = 0, rep_num = 0}).
-record(path, {path, total_clicks = 0, unique_clicks = 0, country_lst = []}).
-record(visitor_ip,{visitor_ip, paths=[]}).



