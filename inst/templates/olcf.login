cd ~/eof/hosvd_code
rm -f .pbdR_server.pbs .pbdR_hostname .pbdR_lnode.sh
cat >> .pbdR_lnode.sh << '..LNODE-EOF..'
cd ~/eof/hosvd_code
cat >> .pbdR_server.pbs << ..PBS-EOF..
{{## PBS script auto-inserted here ##}}
..PBS-EOF..
qsub .pbdR_server.pbs
while [ ! -f .pbdR_hostname ]; do sleep 1; echo -n '.'; done; echo '.' 
ssh -f -L {{lport}}:localhost:{{hport}} -N \$(cat .pbdR_hostname)
echo 'server login node: ' \$(hostname)
echo 'server head node: ' \$(cat .pbdR_hostname)
..LNODE-EOF..
source .pbdR_lnode.sh
