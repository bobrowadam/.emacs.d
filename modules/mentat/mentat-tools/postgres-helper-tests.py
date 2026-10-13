"""Regression checks for the shell operation embedded in postgres.org.

Run: python3 ~/.emacs.d/modules/mentat/mentat-tools/postgres-helper-tests.py
No credentials or service access required.
"""
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
import time
import unittest

SKILL = Path.home() / 'dotfiles/bradwell/skills/postgres.org'
SOURCE = SKILL.read_text().split('(defconst mentat-bradwell-postgres--script', 1)[1]
SOURCE = SOURCE.split('  "One cleaned-up shell operation', 1)[0]
SCRIPT = ''.join(json.loads(match.group()) for match in
                 re.finditer(r'^\s*("(?:[^"\\]|\\.)*")', SOURCE, re.MULTILINE))


class HelperTests(unittest.TestCase):
    def run_helper(self, failure='', query_failure='', cancel=False, stubborn=False):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            tools = root / 'bin'
            tools.mkdir()
            (root / 'infrastructure/environments/prod').mkdir(parents=True)
            query_dir = root / 'query'
            query_dir.mkdir()

            def tool(name, body):
                path = tools / name
                path.write_text('#!/bin/sh\nset -eu\n' + body)
                path.chmod(0o700)

            tool('terraform', '''test -z "${PGPASSWORD-}"
if test "$FAILURE" = sso; then echo 'InvalidGrantException PRIVATE_SENTINEL' >&2; exit 1; fi
case "$4" in
region) echo us-east-1;; aws_profile) echo bradwell-prod;; ec2_instance_id) echo i-test;; rds_endpoint) echo localhost;; rds_database_name) echo test;; rds_port) echo 5432;; rds_master_username) echo postgres;; esac
''')
            tool('aws', '''test -z "${PGPASSWORD-}"
case "$FAILURE" in
expired) echo 'The SSO session associated with this profile has expired or is otherwise invalid PRIVATE_SENTINEL' >&2; exit 1;;
ssm) echo 'TargetNotConnected PRIVATE_SENTINEL' >&2; exit 1;;
permission) echo 'AccessDeniedException PRIVATE_SENTINEL' >&2; exit 1;;
unknown) echo PRIVATE_SENTINEL >&2; exit 1;;
esac
python3 -c 'import json,os,signal,socket,sys,time; signal.signal(signal.SIGTERM, signal.SIG_IGN if os.environ["STUBBORN"] == "1" else signal.SIG_DFL); p=json.loads(sys.argv[1])["localPortNumber"][0]; s=socket.socket(); s.bind(("127.0.0.1",int(p))); s.listen(); open(sys.argv[2],"w").write(str(os.getpid())); time.sleep(60)' "${9}" "$PID_FILE" &
wait $!
''')
            tool('psql', '''test "$PGPASSWORD" = TEST_PASSWORD_SENTINEL
test "$PGCONNECT_TIMEOUT" = 10
if test -n "$QUERY_FAILURE"; then echo "$QUERY_FAILURE PRIVATE_SENTINEL" >&2; exit 1; fi
grep -q 'BEGIN TRANSACTION READ ONLY;' "${16}"
grep -q 'SET LOCAL statement_timeout = 15000;' "${16}"
printf '%s\n' '{"rows":[{"ok":true}],"truncated":false}'
''')
            env = dict(os.environ, PATH=str(tools) + ':' + os.environ['PATH'],
                       PGPASSWORD='TEST_PASSWORD_SENTINEL', FAILURE=failure,
                       QUERY_FAILURE=query_failure, PID_FILE=str(root / 'pid'),
                       STUBBORN='1' if stubborn else '0')
            command = ['/bin/sh', '-c', SCRIPT, '--', str(query_dir), str(root),
                       'prod', '20', '21', '15000', '5000', 'postgres']
            if cancel:
                child = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                         stderr=subprocess.PIPE, text=True, env=env)
                child.stdin.write('SELECT true AS ok')
                child.stdin.close()
                child.stdin = None
                for _ in range(100):
                    if (root / 'pid').exists():
                        break
                    time.sleep(0.01)
                self.assertTrue((root / 'pid').exists(), 'mock tunnel never started')
                child.terminate()
                stdout, stderr = child.communicate(timeout=20)
                result = subprocess.CompletedProcess(command, child.returncode, stdout, stderr)
            else:
                result = subprocess.run(command, input='SELECT true AS ok', text=True,
                                        capture_output=True, env=env, timeout=20)
            self.assertFalse(query_dir.exists(), 'temporary query files survived')
            for sentinel in ('PRIVATE_SENTINEL', 'TEST_PASSWORD_SENTINEL', 'SELECT true'):
                self.assertNotIn(sentinel, result.stdout + result.stderr)
            if (root / 'pid').exists():
                pid = int((root / 'pid').read_text())
                for _ in range(100):
                    try:
                        os.kill(pid, 0)
                    except ProcessLookupError:
                        break
                    time.sleep(0.01)
                with self.assertRaises(ProcessLookupError):
                    os.kill(pid, 0)
            return result

    def test_success(self):
        result = self.run_helper()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(result.stdout), {'rows': [{'ok': True}], 'truncated': False})

    def test_cancellation_cleans_tunnel(self):
        self.assertEqual(self.run_helper(cancel=True).returncode, 143)

    def test_stubborn_plugin_cleanup(self):
        result = self.run_helper(stubborn=True)
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_cancellation_kills_stubborn_plugin(self):
        self.assertEqual(self.run_helper(cancel=True, stubborn=True).returncode, 143)

    def test_expired_sso_session_message(self):
        result = self.run_helper(failure='expired')
        self.assertIn('failed at tunnel: AWS SSO authentication needs renewal', result.stderr)

    def test_sso_failure(self):
        result = self.run_helper(failure='sso')
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('failed at terraform: AWS SSO authentication needs renewal', result.stderr)

    def test_ssm_failure(self):
        result = self.run_helper(failure='ssm')
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('failed at tunnel: SSM target is not connected', result.stderr)

    def test_permission_failure(self):
        self.assertIn('AWS permission denied', self.run_helper(failure='permission').stderr)

    def test_unknown_failure_is_private(self):
        self.assertIn('unclassified failure', self.run_helper(failure='unknown').stderr)

    def test_database_authentication_failure(self):
        self.assertIn('database authentication failed',
                      self.run_helper(query_failure='password authentication failed').stderr)

    def test_statement_timeout(self):
        self.assertIn('query exceeded statement timeout',
                      self.run_helper(query_failure='canceling statement due to statement timeout').stderr)


if __name__ == '__main__':
    unittest.main()
