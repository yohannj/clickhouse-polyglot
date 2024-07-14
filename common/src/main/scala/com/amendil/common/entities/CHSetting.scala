package com.amendil.common.entities

/**
  * ClickHouse setting that can be set in a query
  */
case class CHSettingWithValue[T](setting: CHSetting[T], value: T):
  private val name: String = setting.name

  /**
    * Provide a string in the format that is accepted in a ClickHouse query: `settingName=value`
    */
  def asString: String =
    value match
      case v: Boolean => if v then s"$name=1" else s"$name=0"
      case _          => throw IllegalArgumentException(s"Unable to handle setting of type ${value.getClass}")

/**
  * ClickHouse settings that can be set in a query
  */
enum CHSetting[T](val name: String):
  // Method called apply so that we can write use settings nicely
  // e.g. `CHSetting.AllowDeprecatedErrorProneWindowFunctions(true)`
  def apply(value: T): CHSettingWithValue[T] = CHSettingWithValue(this, value)

  case AllowCustomErrorCodeInThrowif extends CHSetting[Boolean]("allow_custom_error_code_in_throwif")
  case AllowDeprecatedErrorProneWindowFunctions
      extends CHSetting[Boolean]("allow_deprecated_error_prone_window_functions")
  case AllowExperimentalDynamicType extends CHSetting[Boolean]("allow_experimental_dynamic_type")
  case AllowExperimentalFunnelFunctions extends CHSetting[Boolean]("allow_experimental_funnel_functions")
  case AllowExperimentalNlpFunctions extends CHSetting[Boolean]("allow_experimental_nlp_functions")
  case AllowExperimentalObjectType extends CHSetting[Boolean]("allow_experimental_object_type")
  case AllowExperimentalVariantType extends CHSetting[Boolean]("allow_experimental_variant_type")
  case AllowGetClientHttpHeader extends CHSetting[Boolean]("allow_get_client_http_header")
  case AllowIntrospectionFunctions extends CHSetting[Boolean]("allow_introspection_functions")
  case AllowSuspiciousLowCardinalityTypes extends CHSetting[Boolean]("allow_suspicious_low_cardinality_types")
  case DecimalCheckOverflow extends CHSetting[Boolean]("decimal_check_overflow")
  case LogQueries extends CHSetting[Boolean]("log_queries")
