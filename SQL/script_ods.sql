USE [master]
GO

/****** Object:  Database [SAPMEODS]    Script Date: 06.07.2017 14:24:06 ******/
CREATE DATABASE [SAPMEODS]
 CONTAINMENT = NONE
 ON  PRIMARY 
( NAME = N'SAPMEODS', FILENAME = N'O:\DB\SAPMEODS.mdf' , SIZE = 1679360000KB , MAXSIZE = 1679360000KB , FILEGROWTH = 1024KB ),
( NAME = N'SAPMEODS2', FILENAME = N'X:\DB\SAPMEODS2.mdf' , SIZE = 446777344KB , MAXSIZE = UNLIMITED, FILEGROWTH = 1024KB )
 LOG ON 
( NAME = N'SAPMEODS_log', FILENAME = N'R:\LOG\SAPMEODS_log.ldf' , SIZE = 2876352KB , MAXSIZE = 2048GB , FILEGROWTH = 10%)
GO

ALTER DATABASE [SAPMEODS] SET COMPATIBILITY_LEVEL = 90
GO

IF (1 = FULLTEXTSERVICEPROPERTY('IsFullTextInstalled'))
begin
EXEC [SAPMEODS].[dbo].[sp_fulltext_database] @action = 'disable'
end
GO

ALTER DATABASE [SAPMEODS] SET ANSI_NULL_DEFAULT OFF 
GO

ALTER DATABASE [SAPMEODS] SET ANSI_NULLS OFF 
GO

ALTER DATABASE [SAPMEODS] SET ANSI_PADDING OFF 
GO

ALTER DATABASE [SAPMEODS] SET ANSI_WARNINGS OFF 
GO

ALTER DATABASE [SAPMEODS] SET ARITHABORT OFF 
GO

ALTER DATABASE [SAPMEODS] SET AUTO_CLOSE OFF 
GO

ALTER DATABASE [SAPMEODS] SET AUTO_SHRINK OFF 
GO

ALTER DATABASE [SAPMEODS] SET AUTO_UPDATE_STATISTICS ON 
GO

ALTER DATABASE [SAPMEODS] SET CURSOR_CLOSE_ON_COMMIT OFF 
GO

ALTER DATABASE [SAPMEODS] SET CURSOR_DEFAULT  GLOBAL 
GO

ALTER DATABASE [SAPMEODS] SET CONCAT_NULL_YIELDS_NULL OFF 
GO

ALTER DATABASE [SAPMEODS] SET NUMERIC_ROUNDABORT OFF 
GO

ALTER DATABASE [SAPMEODS] SET QUOTED_IDENTIFIER OFF 
GO

ALTER DATABASE [SAPMEODS] SET RECURSIVE_TRIGGERS OFF 
GO

ALTER DATABASE [SAPMEODS] SET  DISABLE_BROKER 
GO

ALTER DATABASE [SAPMEODS] SET AUTO_UPDATE_STATISTICS_ASYNC OFF 
GO

ALTER DATABASE [SAPMEODS] SET DATE_CORRELATION_OPTIMIZATION OFF 
GO

ALTER DATABASE [SAPMEODS] SET TRUSTWORTHY OFF 
GO

ALTER DATABASE [SAPMEODS] SET ALLOW_SNAPSHOT_ISOLATION ON 
GO

ALTER DATABASE [SAPMEODS] SET PARAMETERIZATION SIMPLE 
GO

ALTER DATABASE [SAPMEODS] SET READ_COMMITTED_SNAPSHOT ON 
GO

ALTER DATABASE [SAPMEODS] SET HONOR_BROKER_PRIORITY OFF 
GO

ALTER DATABASE [SAPMEODS] SET RECOVERY SIMPLE 
GO

ALTER DATABASE [SAPMEODS] SET  MULTI_USER 
GO

ALTER DATABASE [SAPMEODS] SET PAGE_VERIFY CHECKSUM  
GO

ALTER DATABASE [SAPMEODS] SET DB_CHAINING OFF 
GO

ALTER DATABASE [SAPMEODS] SET FILESTREAM( NON_TRANSACTED_ACCESS = OFF ) 
GO

ALTER DATABASE [SAPMEODS] SET TARGET_RECOVERY_TIME = 0 SECONDS 
GO

ALTER DATABASE [SAPMEODS] SET  READ_WRITE 
GO

