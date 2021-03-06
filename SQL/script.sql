USE [master]
GO

/****** Object:  Database [SAPMEWIP]    Script Date: 06.07.2017 12:00:36 ******/
CREATE DATABASE [SAPMEWIP]
 CONTAINMENT = NONE
 ON  PRIMARY 
( NAME = N'SAPMEWIP', FILENAME = N'O:\Data\SAPMEWIP.mdf' , SIZE = 644427840KB , MAXSIZE = UNLIMITED, FILEGROWTH = 1024KB )
 LOG ON 
( NAME = N'SAPMEWIP_log', FILENAME = N'R:\Log\SAPMEWIP_log.Ldf' , SIZE = 19131136KB , MAXSIZE = 2048GB , FILEGROWTH = 10%)
GO

ALTER DATABASE [SAPMEWIP] SET COMPATIBILITY_LEVEL = 90
GO

IF (1 = FULLTEXTSERVICEPROPERTY('IsFullTextInstalled'))
begin
EXEC [SAPMEWIP].[dbo].[sp_fulltext_database] @action = 'disable'
end
GO

ALTER DATABASE [SAPMEWIP] SET ANSI_NULL_DEFAULT OFF 
GO

ALTER DATABASE [SAPMEWIP] SET ANSI_NULLS OFF 
GO

ALTER DATABASE [SAPMEWIP] SET ANSI_PADDING OFF 
GO

ALTER DATABASE [SAPMEWIP] SET ANSI_WARNINGS OFF 
GO

ALTER DATABASE [SAPMEWIP] SET ARITHABORT OFF 
GO

ALTER DATABASE [SAPMEWIP] SET AUTO_CLOSE OFF 
GO

ALTER DATABASE [SAPMEWIP] SET AUTO_SHRINK OFF 
GO

ALTER DATABASE [SAPMEWIP] SET AUTO_UPDATE_STATISTICS ON 
GO

ALTER DATABASE [SAPMEWIP] SET CURSOR_CLOSE_ON_COMMIT OFF 
GO

ALTER DATABASE [SAPMEWIP] SET CURSOR_DEFAULT  GLOBAL 
GO

ALTER DATABASE [SAPMEWIP] SET CONCAT_NULL_YIELDS_NULL OFF 
GO

ALTER DATABASE [SAPMEWIP] SET NUMERIC_ROUNDABORT OFF 
GO

ALTER DATABASE [SAPMEWIP] SET QUOTED_IDENTIFIER OFF 
GO

ALTER DATABASE [SAPMEWIP] SET RECURSIVE_TRIGGERS OFF 
GO

ALTER DATABASE [SAPMEWIP] SET  DISABLE_BROKER 
GO

ALTER DATABASE [SAPMEWIP] SET AUTO_UPDATE_STATISTICS_ASYNC OFF 
GO

ALTER DATABASE [SAPMEWIP] SET DATE_CORRELATION_OPTIMIZATION OFF 
GO

ALTER DATABASE [SAPMEWIP] SET TRUSTWORTHY OFF 
GO

ALTER DATABASE [SAPMEWIP] SET ALLOW_SNAPSHOT_ISOLATION ON 
GO

ALTER DATABASE [SAPMEWIP] SET PARAMETERIZATION SIMPLE 
GO

ALTER DATABASE [SAPMEWIP] SET READ_COMMITTED_SNAPSHOT ON 
GO

ALTER DATABASE [SAPMEWIP] SET HONOR_BROKER_PRIORITY OFF 
GO

ALTER DATABASE [SAPMEWIP] SET RECOVERY SIMPLE 
GO

ALTER DATABASE [SAPMEWIP] SET  MULTI_USER 
GO

ALTER DATABASE [SAPMEWIP] SET PAGE_VERIFY CHECKSUM  
GO

ALTER DATABASE [SAPMEWIP] SET DB_CHAINING OFF 
GO

ALTER DATABASE [SAPMEWIP] SET FILESTREAM( NON_TRANSACTED_ACCESS = OFF ) 
GO

ALTER DATABASE [SAPMEWIP] SET TARGET_RECOVERY_TIME = 0 SECONDS 
GO

ALTER DATABASE [SAPMEWIP] SET  READ_WRITE 
GO

