#PBS -lwalltime=72:00:00
#PBS -lselect=1:ncpus=4:mem=64gb

module load anaconda3/personal

cd /rds/general/user/cls1017/projects/hda_students_data/live/Group2/Alicia/Summer_project/Scripts


Rscript 14_obj2_sPLS_BiSelection_results.R

