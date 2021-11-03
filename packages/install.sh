if test ! $(which ruby)
then
    echo "I don't have ruby! Things will be very broken"

    exit 1
else
    ruby install.rb
fi
