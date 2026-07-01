#include <Rcpp.h>
#include <string>

extern "C" {
#include "sgl_to_cgs.h"
#include "cgs.h"
#include "aes.h"
#include "geom.h"
#include "qual.h"
#include "scale.h"
#include "direction.h"
#include "cgs_free.h"
}

std::string r_aes_str(enum aes c_aes_enum) {
	switch(c_aes_enum) {
		case X:
				return "x";
		case Y:
				return "y";
		case RADIUS:
				return "r";
		case THETA:
				return "theta";
		case COLOR:
				return "color";
		case SIZE:
				return "size";
	}
	Rcpp::stop("unexpected aes");
}

Rcpp::List r_col_expr(struct col_expr c_col_expr) {
	std::string r_cta_class;
	switch(c_col_expr.cta) {
		case IDENTITY:
			r_cta_class = "sgl_cta_identity";
			break;
		case AVG:
			r_cta_class = "sgl_cta_avg";
			break;
		case BIN:
			r_cta_class = "sgl_cta_bin";
			break;
		case COUNT:
			r_cta_class = "sgl_cta_count";
			break;
	}
	Rcpp::List r_cta_obj;
	r_cta_obj.attr("class") = Rcpp::CharacterVector::create(r_cta_class, "sgl_cta");

	Rcpp::List result;
	result["column"] = std::string(c_col_expr.column);
	result["cta"] = r_cta_obj;
	if (c_col_expr.arg != NULL) {
		result["arg"] = c_col_expr.arg->value;
	}
	return result;
}

Rcpp::List r_aes_mappings(struct aes_mapping *c_aes_mappings) {
	struct aes_mapping *current = c_aes_mappings;
	Rcpp::List result;
	std::string aes_name;
	while (current != NULL) {
		aes_name = r_aes_str(current->aes);
		result[aes_name] = r_col_expr(current->col_expr);
		current = current->next;
	}
	return result;
}

Rcpp::List r_groupings(struct grouping_expr *c_groupings) {
	struct grouping_expr *current = c_groupings;
	Rcpp::List result;
	while (current != NULL) {
		result.push_back(
			r_col_expr(current->col_expr)
		);
		current = current->next;
	}
	return result;
}

Rcpp::List r_collections(struct collection_expr *c_collections) {
	struct collection_expr *current = c_collections;
	Rcpp::List result;
	while (current != NULL) {
		result.push_back(
			r_col_expr(current->col_expr)
		);
		current = current->next;
	}
	return result;
}

Rcpp::List r_geom_expr(struct geom_expr *c_geom_expr) {
	std::string r_geom_class;
	switch(c_geom_expr->geom) {
		case POINT:
			r_geom_class = "sgl_geom_point";
			break;
		case BAR:
			r_geom_class = "sgl_geom_bar";
			break;
		case LINE:
			r_geom_class = "sgl_geom_line";
			break;
		case BOX:
			r_geom_class = "sgl_geom_box";
			break;
	}
	Rcpp::List r_geom_obj;
	r_geom_obj.attr("class") = Rcpp::CharacterVector::create(r_geom_class, "sgl_geom");

	std::string r_qual_str;
	switch(c_geom_expr->qual) {
		case HORIZONTAL:
			r_qual_str = "horizontal";
			break;
		case JITTERED:
			r_qual_str = "jittered";
			break;
		case REGRESSION:
			r_qual_str = "regression";
			break;
		case UNSTACKED:
			r_qual_str = "unstacked";
			break;
		case VERTICAL:
			r_qual_str = "vertical";
			break;
		case DEFAULT:
			r_qual_str = "default";
			break;
	}

	Rcpp::List result;
	result["geom"] = r_geom_obj;
	result["qual"] = r_qual_str;
	return result;
}

Rcpp::List r_layer(struct layer *c_layer, struct geom_expr *c_geom_expr) {
	Rcpp::List result;
	result["aes_mappings"] = r_aes_mappings(c_layer->aes_mappings);
	result["source_sql_query"] = std::string(c_layer->source_sql_query);
	result["geom_expr"] = r_geom_expr(c_geom_expr);
	if (c_layer->groupings != NULL) {
		result["groupings"] = r_groupings(c_layer->groupings);
	}
	if(c_layer->collections != NULL) {
		result["collections"] = r_collections(c_layer->collections);
	}
	return result;
}

Rcpp::List r_scales(struct scale_expr *c_scales) {
	struct scale_expr *current_scale_expr = c_scales;
	std::string r_scale_class;
	std::string r_aes;
	Rcpp::List result;
	while (current_scale_expr != NULL) {
		Rcpp::List r_scale_obj;
		switch(current_scale_expr->scale) {
			case LINEAR:
				r_scale_class = "sgl_scale_linear";
				break;
			case LN:
				r_scale_class = "sgl_scale_ln";
				break;
			case LOG:
				r_scale_class = "sgl_scale_log";
				break;
		}
		r_scale_obj.attr("class") = Rcpp::CharacterVector::create(r_scale_class, "sgl_scale");
		r_aes = r_aes_str(current_scale_expr->aes);

		result[r_aes] = r_scale_obj;

		current_scale_expr = current_scale_expr->next;
	}
	return result;
}

Rcpp::List r_facets(struct facet_expr *c_facets) {
	std::string column_name;
	std::string r_facet_direction;
	Rcpp::List result;
	struct facet_expr *current_facet_expr = c_facets;
	while (current_facet_expr != NULL) {
		Rcpp::List facet_entry;
		switch(current_facet_expr->direction) {
			case DEFAULT_DIRECTION:
				r_facet_direction = "default";
				break;
			case HORIZONTAL_DIRECTION:
				r_facet_direction = "horizontal";
				break;
			case VERTICAL_DIRECTION:
				r_facet_direction = "vertical";
				break;
		}
		facet_entry["direction"] = r_facet_direction;
		facet_entry["column"] = std::string(current_facet_expr->column);

		result.push_back(facet_entry);

		current_facet_expr = current_facet_expr->next;
	}
	return result;
}

Rcpp::List r_titles(struct title_expr *c_titles) {
	std::string r_aes;
	Rcpp::List result;
	struct title_expr *current_title_expr = c_titles;
	while (current_title_expr != NULL) {
		r_aes = r_aes_str(current_title_expr->aes);
		result[r_aes] = std::string(current_title_expr->title);

		current_title_expr = current_title_expr->next;
	}
	return result;
}

// [[Rcpp::export]]
Rcpp::List sgl_to_rgs(std::string sgl_stmt) {
	struct cgs *cgs = (struct cgs *)malloc(sizeof(struct cgs));
	char *errmsg = NULL;

	sgl_to_cgs(const_cast<char*>(sgl_stmt.c_str()), cgs, &errmsg);

	if (errmsg != NULL) {
		std::string error_message(errmsg);
		free(errmsg);
		free_cgs(cgs);
		Rcpp::stop(error_message);
	}

	Rcpp::List result;
	Rcpp::List layers;

	struct layer *current_layer = cgs->layers;
	struct geom_expr *current_geom_expr;
	while(current_layer != NULL) {
		current_geom_expr = current_layer->geoms;
		while(current_geom_expr != NULL) {
			layers.push_back(r_layer(current_layer, current_geom_expr));
			current_geom_expr = current_geom_expr->next;
		}
		current_layer = current_layer->next;
	}

	result["layers"] = layers;

	if (cgs->scales != NULL) {
		result["scales"] = r_scales(cgs->scales);
	}

	if (cgs->facets != NULL) {
		result["facets"] = r_facets(cgs->facets);
	}

	if (cgs->titles != NULL) {
		result["titles"] = r_titles(cgs->titles);
	}

	free_cgs(cgs);

	return result;
}
