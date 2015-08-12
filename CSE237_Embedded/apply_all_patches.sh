#!/bin/bash

WORK_FOLDER=/home/prodromou/Desktop/classes/CSE237_Embedded/WORKING_DIRECTORY
PATCHDIR=/home/prodromou/Downloads/2013_cse237a_dvfs_localmachine/patch_directory/patches_to_be_applied
#echo "WORK_FOLDER: $WORK_FOLDER"
#echo "PATCHDIR: $PATCHDIR"

read -p "Apply patch 1? [ENTER] " button
cd $WORK_FOLDER/device/qcom/msm8660_surf
patch -p1 < $PATCHDIR/0001-patch-for-CAF-and-device-build.patch

read -p "Apply patch 2? [ENTER] " button
cd $WORK_FOLDER/bootable/bootloader/lk
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/bootable/bootloader/lk/platform/0001-msm8060-config-charger-maximum-current-in-dragonboar.patch

read -p "Apply patch 3? [ENTER] " button
cd $WORK_FOLDER/bootable/bootloader/lk
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/bootable/bootloader/lk/target/0001-msm8060-config-charger-maximum-current-in-dragonboar.patch

read -p "Apply patch 4? [ENTER] " button
cd $WORK_FOLDER/device/qcom/common/
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/device/0001-8060dragon_device_common_mk.patch

read -p "Apply patch 5? [ENTER] " button
cd $WORK_FOLDER/frameworks/base
patch -p1 < /$PATCHDIR/combinedbuild/8660_8060_Common/frameworks/0001-HomeButton.patch

read -p "Apply patch 6? [ENTER] " button
cd $WORK_FOLDER/hardware/qcom/alsa_sound
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/hardware/0001-8060dragon_hardware_alsa_sound_androidmk.patch

read -p "Apply patch 7? [ENTER] " button
cd $WORK_FOLDER/hardware/qcom/alsa_sound
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/hardware/0001-8060dragon_hardware_alsa_sound_audiohw.patch

read -p "Apply patch 8? [ENTER] " button
cd $WORK_FOLDER/hardware/qcom/media
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/hardware/0001-8060dragon_hardware_media_audio_androidmk.patch

read -p "Apply patch 9? [ENTER] " button
cd $WORK_FOLDER/hardware/qcom/camera
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/hardware/0001-8660_camera_camcoder_hw-and-camera-HDMI-LCD-preview.patch

read -p "Apply patch 10? [ENTER] " button
cd $WORK_FOLDER/hardware/qcom/alsa_sound
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/hardware/0001-dragon_hardware_alsa_sound_alsadefault.patch

read -p "Apply patch 11? [ENTER] " button
cd $WORK_FOLDER/kernel
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/kernel/arch/0001-msm8060-enable-charger-in-dragonboard03.patch

read -p "Apply patch 12? [ENTER] " button
cd $WORK_FOLDER/kernel
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/kernel/0001-8060dragon_kernel_arch_machmsm_device8x60_hdmi.patch

read -p "Apply patch 13? [ENTER] " button
cd $WORK_FOLDER/kernel
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/kernel/0001-8060dragon_kernel_msm8660_apq_wm903_hdmi_fix.patch

read -p "Apply patch 14? [ENTER] " button
cd $WORK_FOLDER/kernel
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/kernel/0001-8060dragon_kernel_hdmi_audio_config.patch

read -p "Apply patch 15? [ENTER] " button
cd $WORK_FOLDER/kernel
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/kernel/0001-8060dragon_msm8660-apq-wm903.patch

read -p "Apply patch 16? [ENTER] " button
cd $WORK_FOLDER/kernel
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/kernel/0001-8060dragon_wm8903.patch

read -p "Apply patch 17? [ENTER] " button
cd $WORK_FOLDER/kernel
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/kernel/0001-8060dragon_wm8903_header.patch

read -p "Apply patch 18? [ENTER] " button
cd $WORK_FOLDER/kernel
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/kernel/0001-camera_kernel.patch

read -p "Apply patch 19? [ENTER] " button
cd $WORK_FOLDER/kernel
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/kernel/0001-dragonboard_sensor_gb.patch

read -p "Apply patch 20? [ENTER] " button
cd $WORK_FOLDER/packages/apps/Camera
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/packages/0001-camera_app.patch

read -p "Apply patch 21? [ENTER] " button
cd $WORK_FOLDER/system/core
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/system/0001-camera_sensor_dragon_system_init.patch

read -p "Apply patch 22? [ENTER] " button
cd $WORK_FOLDER/vendor
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/vendor/0001-patch_with_changes_8060dragon_vendor_kerneltest_liba.patch

read -p "Apply patch 23? [ENTER] " button
cd $WORK_FOLDER/vendor
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/vendor/0001-8060dragon_vendor_kerneltest_libalsa-intf_HiFi_Rec_WM8903.patch

read -p "Apply patch 24? [ENTER] " button
cd $WORK_FOLDER/vendor
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/vendor/0001-8060dragon_vendor_kerneltest_libalsa-intf_HiFi_WM890.patch

read -p "Apply patch 25? [ENTER] " button
cd $WORK_FOLDER/vendor/qcom/opensource/kernel-tests/mm-audio
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/vendor/0001-8060dragon_vendor_kerneltest_libalsa-intf_mk.patch

read -p "Apply patch 26? [ENTER] " button
cd $WORK_FOLDER/vendor
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/vendor/0001-8060dragon_vendor_kerneltest_libalsa-intf_soc_msm_WM8903.patch

read -p "Apply patch 27? [ENTER] " button
cd $WORK_FOLDER/vendor/qcom/opensource/kernel-tests/mm-audio
patch -p1 < $PATCHDIR/combinedbuild/8660_8060_Common/vendor/0001-8060dragon_vendor_kerneltest_libalsa-intf_usecases.patch

read -p "Apply patch 28? [ENTER] " button
cd $WORK_FOLDER/kernel
patch -p1 < $PATCHDIR/Switch_to_per_cpu_IRQ_framework.patch

read -p "Apply patch 29? [ENTER] " button
cd $WORK_FOLDER/kernel
patch -p1 < $PATCHDIR/spinlock.patch

read -p "Apply patch 30? [ENTER] " button
cd $WORK_FOLDER/kernel
patch -p1 < $PATCHDIR/perf_event_mask.patch

read -p "Apply patch 31? [ENTER] " button
cd $WORK_FOLDER/kernel
patch -p1 < $PATCHDIR/dont_reset_pmu.patch

read -p "Replace file sched.c? [ENTER] " button
mv $WORK_FOLDER/kernel/kernel/sched.c $WORK_FOLDER/kernel/kernel/sched_original.c
cp /home/prodromou/Downloads/2013_cse237a_dvfs_localmachine/extra_sources/sched.c $WORK_FOLDER/kernel/kernel/sched.c

read -p "Replace file acpuclock-arm11.c? [ENTER] " button
mv $WORK_FOLDER/kernel/arch/arm/mach-msm/acpuclock-arm11.c $WORK_FOLDER/kernel/arch/arm/mach-msm/acpuclock-arm11_original.c
cp /home/prodromou/Downloads/2013_cse237a_dvfs_localmachine/extra_sources/acpuclock-arm11.c $WORK_FOLDER/kernel/arch/arm/mach-msm/acpuclock-arm11.c

read -p "Replace file acpuclock-8x60.c? [ENTER] " button
mv $WORK_FOLDER/kernel/arch/arm/mach-msm/acpuclock-8x60.c $WORK_FOLDER/kernel/arch/arm/mach-msm/acpuclock-8x60_original.c
cp /home/prodromou/Downloads/2013_cse237a_dvfs_localmachine/extra_sources/acpuclock-8x60.c $WORK_FOLDER/kernel/arch/arm/mach-msm/acpuclock-8x60.c

echo "Script done."
cd $WORK_FOLDER
