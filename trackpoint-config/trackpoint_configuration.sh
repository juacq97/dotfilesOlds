#! /bin/sh

# Desactiva  press-to-select
echo -n 0 > /sys/devices/platform/i8042/serio1/serio2/press_to_select

# Configura sensibilidad y velocidad
echo '130' > /sys/devices/platform/i8042/serio1/serio2/sensitivity
echo '50' > /sys/devices/platform/i8042/serio1/serio2/speed
