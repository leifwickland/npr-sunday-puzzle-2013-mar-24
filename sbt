FILE="$(dirname $0)/.sbt-extras.sh"
if [ ! -f $FILE ]; then 
  URL="https://raw.github.com/paulp/sbt-extras/master/sbt"
  echo "Downloading $URL..."
  curl -k $URL -o $FILE
  chmod +x $FILE
fi
exec $FILE $*
