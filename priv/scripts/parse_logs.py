import datetime
import os
import subprocess

from erlport import Atom, Port, Protocol, String
import pygeoip


class ParseEval(Protocol):
    """Parses the webmachine access log for usage statistics of shortened URLs"""

    def __init__(self, log_dir=None):
        super(ParseEval, self).__init__()
        self.log_dir = log_dir or os.path.join(os.path.abspath(os.getcwd()), 
                                               'priv',
                                               'log')

    def handle_path(self, path):
        # parse the log files from the last hour
        log_timestamp = datetime.datetime.utcnow() - datetime.timedelta(hours=1) 
        log_fname = 'access.log.%d_%02d_%02d_%02d' % (log_timestamp.year, 
                                                      log_timestamp.month, 
                                                      log_timestamp.day, 
                                                      log_timestamp.hour)
        log_fpath = os.path.join(self.log_dir, log_fname)
        if not os.path.exists(log_fpath):
            return Atom('reschedule'), log_timestamp.minute + 1

        parse_cmd = 'grep -e \'GET /%s \' %s | awk \'{ print $1 }\'' % (String(path), 
                                                                        log_fpath)
        gi = pygeoip.GeoIP('/usr/local/GeoIP/GeoIP.dat')
        proc = subprocess.Popen(parse_cmd, shell=True, stdout=subprocess.PIPE)
        stdout, stderr = proc.communicate()
        total_visits = 0
        countries = set()
        ips = set()
        for ip in stdout.splitlines():
            countries.add(gi.country_code_by_addr(ip))
            ips.add(ip)
            total_visits += 1
        return list(countries), list(ips), total_visits, log_timestamp


if __name__ == "__main__":
    pe = ParseEval()
    pe.run(Port(use_stdio=True))
