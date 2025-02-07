-- phpMyAdmin SQL Dump
-- version 3.5.1
-- http://www.phpmyadmin.net
--
-- 主机: localhost
-- 生成日期: 2025 年 02 月 07 日 02:13
-- 服务器版本: 5.7.17-log
-- PHP 版本: 5.3.13

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
SET time_zone = "+00:00";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- 数据库: `agv`
--

-- --------------------------------------------------------

--
-- 表的结构 `task_history`
--

CREATE TABLE IF NOT EXISTS `task_history` (
  `task_type` varchar(20) COLLATE utf8_unicode_ci NOT NULL,
  `amr_pos_x` int(11) DEFAULT NULL,
  `amr_pos_y` int(11) DEFAULT NULL,
  `amr_pos_z` int(11) DEFAULT NULL,
  `amr_pos_theta` int(11) DEFAULT NULL,
  `amr_tag_id` int(11) DEFAULT NULL,
  `ftp_url` varchar(300) COLLATE utf8_unicode_ci NOT NULL,
  `stitch_state` varchar(10) COLLATE utf8_unicode_ci DEFAULT 'none' COMMENT 'stitch ok/ng/none',
  `requestor` varchar(20) COLLATE utf8_unicode_ci DEFAULT '',
  `task_time` datetime DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

DELIMITER $$
--
-- 事件
--
CREATE DEFINER=`root`@`localhost` EVENT `charger_his_del_event` ON SCHEDULE EVERY 1 DAY STARTS '2022-08-11 08:13:45' ON COMPLETION NOT PRESERVE ENABLE DO BEGIN
delete FROM agv.charger_history WHERE `cur_time` < SYSDATE() - INTERVAL 14 DAY;
END$$

DELIMITER ;

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
