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
}

// [[Rcpp::export]]
Rcpp::List sgl_to_rgs(std::string sgl_stmt) {
    struct cgs cgs;
		cgs.layers = NULL;
		cgs.scales = NULL;
		cgs.facets= NULL;
		cgs.titles= NULL;
    char *errmsg = NULL;
		struct layer *current_layer;
		struct layer *next_layer;
		struct scale_expr *current_scale_expr;
		struct scale_expr *next_scale_expr;
		struct facet_expr *current_facet_expr;
		struct facet_expr *next_facet_expr;
		struct title_expr *current_title_expr;
		struct title_expr *next_title_expr;

    // Call the C parser function
    sgl_to_cgs(const_cast<char*>(sgl_stmt.c_str()), &cgs, &errmsg);
   
    // Check for errors
    if (errmsg != NULL) {
        std::string error_message(errmsg);
        free(errmsg);
        Rcpp::stop(error_message);
    }
    
    // Convert C struct to R list
 		Rcpp::List result;
		Rcpp::List layers;

		current_layer = cgs.layers;
  
		while(current_layer != NULL) { 
			next_layer = current_layer->next;
			Rcpp::List layer;
 
			// Add SQL query
			if (current_layer->source_sql_query != NULL) {
					layer["source_sql_query"] = std::string(current_layer->source_sql_query);
			} else {
					layer["source_sql_query"] = R_NilValue;
			}
			
			// Convert aesthetic mappings to R list
			Rcpp::List aes_list;
			struct aes_mapping *current = current_layer->aes_mappings;
			while (current != NULL) {
					std::string aes_name;
					switch(current->aes) {
							case X:
									aes_name = "x";
									break;
							case Y:
									aes_name = "y";
									break;
							case RADIUS:
									aes_name = "r";
									break;
							case THETA:
									aes_name = "theta";
									break;
							case COLOR:
									aes_name = "color";
									break;
							case SIZE:
									aes_name = "size";
									break;
							default:
									aes_name = "unknown";
					}

					Rcpp::List cta_obj;
					switch(current->col_expr.cta) {
							case IDENTITY:
								cta_obj.attr("class") = Rcpp::CharacterVector::create("sgl_cta_identity", "sgl_cta");
								break;
							case AVG:
								cta_obj.attr("class") = Rcpp::CharacterVector::create("sgl_cta_avg", "sgl_cta");
								break;
							case BIN:
								cta_obj.attr("class") = Rcpp::CharacterVector::create("sgl_cta_bin", "sgl_cta");
								break;
							case COUNT:
								cta_obj.attr("class") = Rcpp::CharacterVector::create("sgl_cta_count", "sgl_cta");
								break;
							default:
								cta_obj.attr("class") = Rcpp::CharacterVector::create("sgl_cta");
					}

					std::string column = std::string(current->col_expr.column);
					Rcpp::List col_expr;
					col_expr["column"] = column;
					col_expr["cta"] = cta_obj;
					if (current->col_expr.arg != NULL) {
						col_expr["arg"] = current->col_expr.arg->value;
					}
					aes_list[aes_name] = col_expr;
					
					current = current->next;
			}
			layer["aes_mappings"] = aes_list;

			// Convert groupings to R list
			if(current_layer->groupings != NULL) {
				Rcpp::List grouping_list;
				struct grouping_expr *current_grouping = current_layer->groupings;
				while (current_grouping != NULL) {
						std::string column = std::string(current_grouping->col_expr.column);

						Rcpp::List cta_obj;
						switch(current_grouping->col_expr.cta) {
								case IDENTITY:
									cta_obj.attr("class") = Rcpp::CharacterVector::create("sgl_cta_identity", "sgl_cta");
									break;
								case AVG:
									cta_obj.attr("class") = Rcpp::CharacterVector::create("sgl_cta_avg", "sgl_cta");
									break;
								case BIN:
									cta_obj.attr("class") = Rcpp::CharacterVector::create("sgl_cta_bin", "sgl_cta");
									break;
								case COUNT:
									cta_obj.attr("class") = Rcpp::CharacterVector::create("sgl_cta_count", "sgl_cta");
									break;
								default:
									cta_obj.attr("class") = Rcpp::CharacterVector::create("sgl_cta");
						}

						Rcpp::List col_expr;
						col_expr["column"] = column;
						col_expr["cta"] = cta_obj;
						if (current_grouping->col_expr.arg != NULL) {
							col_expr["arg"] = current_grouping->col_expr.arg->value;
						}

						grouping_list.push_back(col_expr);
						
						current_grouping = current_grouping->next;
				}
				layer["groupings"] = grouping_list;
			}

			// Convert collections to R list
			if(current_layer->collections != NULL) {
				Rcpp::List collection_list;
				struct collection_expr *current_collection = current_layer->collections;
				while (current_collection != NULL) {
						std::string column = std::string(current_collection->col_expr.column);

						Rcpp::List cta_obj;
						switch(current_collection->col_expr.cta) {
							case IDENTITY:
								cta_obj.attr("class") = Rcpp::CharacterVector::create("sgl_cta_identity", "sgl_cta");
								break;
							case AVG:
								cta_obj.attr("class") = Rcpp::CharacterVector::create("sgl_cta_avg", "sgl_cta");
								break;
							case BIN:
								cta_obj.attr("class") = Rcpp::CharacterVector::create("sgl_cta_bin", "sgl_cta");
								break;
							case COUNT:
								cta_obj.attr("class") = Rcpp::CharacterVector::create("sgl_cta_count", "sgl_cta");
								break;
							default:
								cta_obj.attr("class") = Rcpp::CharacterVector::create("sgl_cta");
						}

						Rcpp::List col_expr;
						col_expr["column"] = column;
						col_expr["cta"] = cta_obj;
						if (current_collection->col_expr.arg != NULL) {
							col_expr["arg"] = current_collection->col_expr.arg->value;
						}

						collection_list.push_back(col_expr);
						
						current_collection = current_collection->next;
				}
				layer["collections"] = collection_list;
			}

			// Convert geoms to geom list
			Rcpp::List geom_list;
			struct geom_expr *current_geom_expr = current_layer->geoms;
			while(current_geom_expr != NULL) {
				Rcpp::List geom_obj;
				enum geom geom_enum = current_geom_expr->geom;
				switch(geom_enum) {
						case POINT:
								geom_obj.attr("class") = Rcpp::CharacterVector::create("sgl_geom_point", "sgl_geom");
								break;
						case BAR:
								geom_obj.attr("class") = Rcpp::CharacterVector::create("sgl_geom_bar", "sgl_geom");
								break;
						case LINE:
								geom_obj.attr("class") = Rcpp::CharacterVector::create("sgl_geom_line", "sgl_geom");
								break;
						case BOX:
								geom_obj.attr("class") = Rcpp::CharacterVector::create("sgl_geom_box", "sgl_geom");
								break;
						default:
								geom_obj.attr("class") = Rcpp::CharacterVector::create("sgl_geom");
				}

				// Convert qual enum
				enum qual qual_enum = current_geom_expr->qual;
				std::string qual_str;
				switch(qual_enum) {
						case HORIZONTAL:
							qual_str = "horizontal";
							break;
						case JITTERED:
							qual_str = "jittered";
							break;
						case REGRESSION:
							qual_str = "regression";
							break;
						case UNSTACKED:
							qual_str = "unstacked";
							break;
						case VERTICAL:
							qual_str = "vertical";
							break;
						case DEFAULT:
							qual_str = "default";
							break;
						default:
							qual_str = "unknown";
				}
			 
				Rcpp::List geom_expr;
				// Add geom_expr
				geom_expr["qual"] = qual_str;
				geom_expr["geom"] = geom_obj;
				geom_list.push_back(geom_expr);
				current_geom_expr = current_geom_expr->next;
			}	

			// Add layer for each geom expr
			for (int i = 0; i < geom_list.size(); i++) {
				layer["geom_expr"] = geom_list[i];
				layers.push_back(Rcpp::clone(layer));
			}

			// Clean up allocated memory
			if (current_layer->source_sql_query != NULL) {
					free(current_layer->source_sql_query);
			}
			
			// Free the linked list of aes_mappings
			current = current_layer->aes_mappings;
			while (current != NULL) {
					struct aes_mapping *next = current->next;
					free(current->col_expr.column);
					free(current->col_expr.arg);
					free(current);
					current = next;
			}

			// Free the linked list of geoms
			struct geom_expr *current_geom = current_layer->geoms;
			struct geom_expr *next_geom; 
			while (current_geom != NULL) {
					next_geom = current_geom->next;
					free(current_geom);
					current_geom = next_geom;
			}

			// Free the linked list of groupings
			struct grouping_expr *current_grouping = current_layer->groupings;
			struct grouping_expr *next_grouping;
			while (current_grouping != NULL) {
					next_grouping = current_grouping->next;
					free(current_grouping->col_expr.column);
					free(current_grouping->col_expr.arg);
					free(current_grouping);
					current_grouping = next_grouping;
			}

			// Free the linked list of collections 
			struct collection_expr *current_collection = current_layer->collections;
			struct collection_expr *next_collection;
			while (current_collection != NULL) {
					next_collection = current_collection->next;
					free(current_collection->col_expr.column);
					free(current_collection->col_expr.arg);
					free(current_collection);
					current_collection = next_collection;
			}

			free(current_layer);
			current_layer = next_layer;
		}

		result["layers"] = layers;

		if (cgs.scales != NULL) {	
			Rcpp::List scale_list;
			current_scale_expr = cgs.scales;
			while (current_scale_expr != NULL) {
				next_scale_expr = current_scale_expr->next;

				Rcpp::List scale_obj;
				switch(current_scale_expr->scale) {
						case LINEAR:
								scale_obj.attr("class") = Rcpp::CharacterVector::create("sgl_scale_linear", "sgl_scale");
								break;
						case LOG:
								scale_obj.attr("class") = Rcpp::CharacterVector::create("sgl_scale_log", "sgl_scale");
								break;
						default:
								scale_obj.attr("class") = Rcpp::CharacterVector::create("sgl_scale");
				}

				std::string scale_aes;
				switch(current_scale_expr->aes) {
						case X:
								scale_aes = "x";
								break;
						case Y:
								scale_aes = "y";
								break;
						case RADIUS:
								scale_aes = "r";
								break;
						case THETA:
								scale_aes = "theta";
								break;
						case COLOR:
								scale_aes = "color";
								break;
						case SIZE:
								scale_aes = "size";
								break;
						default:
								scale_aes = "unknown";
				}
				
				scale_list[scale_aes] = scale_obj;				
	
				free(current_scale_expr);
				current_scale_expr = next_scale_expr;
			}
			result["scales"] = scale_list;
		}

		if (cgs.facets != NULL) {	
			Rcpp::List facet_list;
			current_facet_expr = cgs.facets;
			while (current_facet_expr != NULL) {
				next_facet_expr = current_facet_expr->next;

				Rcpp::List facet_entry;
				std::string column_name = current_facet_expr->column;
				std::string facet_direction;
				switch(current_facet_expr->direction) {
						case DEFAULT_DIRECTION:
								facet_direction = "default";
								break;
						case HORIZONTAL_DIRECTION:
								facet_direction = "horizontal";
								break;
						case VERTICAL_DIRECTION:
								facet_direction = "vertical";
								break;
						default:
								facet_direction = "unknown";
				}

				facet_entry["column"] = column_name;
				facet_entry["direction"] = facet_direction;			

				facet_list.push_back(facet_entry);

				free(current_facet_expr->column);	
				free(current_facet_expr);
				current_facet_expr = next_facet_expr;
			}
			result["facets"] = facet_list;
		}

		if (cgs.titles != NULL) {	
			Rcpp::List title_list;
			current_title_expr = cgs.titles;
			while (current_title_expr != NULL) {
				next_title_expr = current_title_expr->next;

				std::string title_aes;
				switch(current_title_expr->aes) {
						case X:
								title_aes = "x";
								break;
						case Y:
								title_aes = "y";
								break;
						case RADIUS:
								title_aes = "r";
								break;
						case THETA:
								title_aes = "theta";
								break;
						case COLOR:
								title_aes = "color";
								break;
						case SIZE:
								title_aes = "size";
								break;
						default:
								title_aes = "unknown";
				}
				
				std::string title = current_title_expr->title;
				title_list[title_aes] = title;				

				free(current_title_expr->title);	
				free(current_title_expr);
				current_title_expr = next_title_expr;
			}
			result["titles"] = title_list;
		}

    return result;
}
